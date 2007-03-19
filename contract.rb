require 'inheritable_attributes'

class Class
  # Redefines each of the given methods as a call to self#send.  This assumes
  # that self#send knows what do with them.
  def proxy_self(*meths)
    meths.each do |meth|
      class_eval <<-EOS
        def #{meth}(*args, &block)
          self.send(:#{meth}, *args, &block)
        end
      EOS
    end
    nil
  end
end

# A module for defining class and method contracts (as in design-by-contract
# programming).  See Contract::ClassMethods for full documentation.
module Contract
  def Contract.included(base)
    base.extend(ClassMethods)
    base.extend(ClauseMethods)
    base.send(:include, Contract::InstanceMethods)

    base.class_inheritable_array :invariants
    base.write_inheritable_array :invariants, []

    base.class_inheritable_hash :checked_methods
    base.write_inheritable_hash :checked_methods, {}

    base.class_inheritable_hash :checked_filters
    base.write_inheritable_hash :checked_filters, {}

    class << base
      alias :instantiate :new
      # Override the class-level new method of every class that includes
      # Contract and cause it to return a proxy object for the original.
      def new(*args, &block)
        # Special case:  check invariants for constructor.
        if checked_methods.has_key? :initialize
          begin
            checked_methods[:initialize].check_accepts(*args, &block)
          rescue Exception => e
            raise e.class, e.message
          end
        end

        o = self.instantiate(*args, &block)
        o.check_invariants
        Proxy.new( o )
      end
    end
  end

  # Define method and invariant contracts on a class.  Invariant and method
  # contracts are conditions that are checked before and/or after the
  # execution of each method in the class.  They are inherited by subclasses
  # and are applied to all superclass methods.
  #
  # == Specifying an invariant
  #
  # Invariants are specified with a block and an optional message string:
  #
  #   invariant("Cannot be empty") { not empty ? }
  #
  # You may specify as many invariants as you wish and they will each be
  # checked before and after the execution of each method.
  #
  # == Specifying a method contract
  #
  # Method contracts allow you to set constraints on the arguments accepted
  # by a method and its return value(s).  Any object which defines a
  # <tt>===</tt> method may be specified as a constraint.  In the class of
  # classes, this is equivalent to is_a?:
  #
  #   String === "foo" <=> "foo".is_a? String
  #
  # Other examples of classes with useful predefined <tt>===</tt> methods
  # include Regexp and Range.
  #
  # The syntax for defining a method contract is as follows:
  #
  #   method :foo, :accepts => [ String, Integer ], :returns => [ String ]
  #
  # The first symbol, :foo, may be omitted.  In this case the method contract
  # will be applied to the next method definition following the contract.
  #
  #   method :accepts => [ String ]
  #   def foo(str)
  #     ...
  #   end
  #
  # Variable arguments may be enclosed in brackets as an array.  Blocks
  # should be specified using the Contract::Block object.
  #
  #   method :accepts => [ String, [ Integer ], Contract::Block ]
  #
  # == Order of execution
  #
  # The order of execution for contract checking is as follows:
  #
  # * the invariants are checked
  # * the method arguments are checked against its :accepts contract, if any
  # * the method itself is executed
  # * the method's return value(s) are checked against its :returns contract,
  #   if any
  # * the invariants are checked yet again
  #
  module ClassMethods
    # Specify an invariant, with a block and an optional error message.
    def invariant(mesg=nil, &block)
      write_inheritable_array(:invariants, [ Invariant.new(mesg, &block) ] )
      nil
    end
    alias :always :invariant
    
    # Specify a method contract, with a hash and an optional method name.  If
    # no method name is given the contract will be applied to the next defined
    # method.
    def method(meth_or_options, options={})
      if meth_or_options.is_a? Hash
        defer_checked_method(meth_or_options)
      else
        add_checked_method(meth_or_options, options)
      end
      nil
    end

    def before(meth_name_or_mesg=nil, mesg=nil, &block)
      evaluate_filter(:before, meth_name_or_mesg, mesg, &block)
    end
    alias :pre :before

    def after(meth_name_or_mesg=nil, mesg=nil, &block)
      evaluate_filter(:after, meth_name_or_mesg, mesg, &block)
    end
    alias :post :after

    def around(meth_name_or_mesg=nil, mesg=nil, &block)
      [ :after, :before ].each do |check_when|
        evaluate_filter(check_when, meth_name_or_mesg, mesg, &block)
      end
    end

    # Callback from method add event.  If a previous method contract
    # declaration was deferred, complete it now with the name of the newly-
    # added method.
    def method_added(meth_name)
      add_checked_method(meth_name, @deferred_options) unless @deferred_options.nil?
      unless @deferred_filters.nil? || @deferred_filters.empty?
        @deferred_filters.each do |f|
          f.meth_name = meth_name
          add_filter(f)
        end
      end
      @deferred_options, @deferred_filters = nil
      super(meth_name)
    end

    private
      def add_checked_method(meth_name, options)
        write_inheritable_hash :checked_methods, { meth_name => Method.new(meth_name, options) }
      end
      def defer_checked_method(options)
        @deferred_options = options
      end
      def evaluate_filter(check_when, meth_or_mesg=nil, mesg=nil, &block)
        if meth_or_mesg.is_a?(String) || meth_or_mesg.nil? # defer filter declaration
          ( @deferred_filters ||= [] ) << Filter.new(nil, check_when, meth_or_mesg, &block)
        else
          add_filter Filter.new(meth_or_mesg, check_when, mesg, &block)
        end
        nil
      end
      def add_filter(filter)
        new_filters = ( checked_filters[filter.meth_name] || [] ) << filter
        write_inheritable_hash :checked_filters, { filter.meth_name => new_filters }
      end
  end

  module InstanceMethods
    def check_invariants
      self.class.invariants.each do |invar|
        unless invar.holds?(self)
          mesg = invar.mesg || "Invariant check failed"
          raise ContractViolation, mesg
        end
      end
    end
    def check_filters(filters, *args, &block)
      filters.find_all(&block).each do |f|
        # Bind the block to the object context to get access to ivars, call
        # it, and remove it.  Awkward.
        self.class.instance_eval do
          define_method(:bound_filter_passes?, &(f.block))
        end
        raise(ContractViolation, f.mesg) unless bound_filter_passes?(*args)
        self.class.send(:remove_method, :bound_filter_passes?)
      end
    end
  end

  # This class filters all method calls to its proxied object through any
  # contracts defined on that object's class.  It attempts to look and act
  # like its proxied object for all intents and purposes, although it notably
  # does not proxy __id__, __send__, or class.
  class Proxy
    NOT_PROXIED  = [ "__id__", "__send__", "class" ]
    SELF_PROXIED = Object.instance_methods - NOT_PROXIED

    proxy_self *SELF_PROXIED

    # any -> Proxy
    def initialize(proxied)
      @proxied = proxied
    end

    # -> any
    # Returns the unchecked, wrapped object.
    def unchecked!
      @proxied
    end

    # -> Class
    def proxied_class
      @proxied.class
    end

    # Override the send method, and alias method_missing to same.
    # This method intercepts all method calls and runs them through the
    # contract filter.
    def send(meth_name, *args, &block)
      begin
        @proxied.check_invariants
        method_contract = @proxied.class.checked_methods[meth_name]
        filters = @proxied.class.checked_filters[meth_name] || []

        method_contract.check_accepts(*args, &block) unless method_contract.nil?
        @proxied.check_filters(filters, *args) {|f| f.before?}

        return_val = @proxied.send meth_name, *args, &block
        method_contract.check_returns(*return_val) unless method_contract.nil?

        @proxied.check_filters(filters, *(args << return_val)) {|f| f.after?}

        @proxied.check_invariants
        return_val
      rescue Exception => e
        # Reraise in the local context for a more useful stack trace.
        raise e.class, e.message
      end
    end
    alias :method_missing :send


  end

  # This class defines a class invariant, which has a block and an optional
  # method.
  class Invariant
    def initialize(mesg=nil, &block)
      @mesg = mesg
      @block = block
    end
    # Any -> Boolean
    # Evaluates this class's block in the binding of the given object.
    def holds?(o)
      block = @block
      o.instance_eval &block
    end
    def mesg
      @mesg || "Invariant check failed"
    end
  end

  class Filter
    attr_accessor :meth_name, :check_when, :block
    def initialize(meth_name, check_when, mesg=nil, &block)
      @meth_name = meth_name
      @check_when = check_when
      @mesg = mesg
      @block = block
    end
    def before?; @check_when == :before; end
    def after?;  @check_when == :after;  end
    def holds?(*args)
      @block.call(*args)
    end
    def mesg
      @mesg || "Filter check failed"
    end
  end

  # This class defines a method contract, which has a method name and
  # (optionally) a list of constraints on the things it accepts and returns,
  # specified with :accepts => [ ... ] and :returns => [ ... ].
  class Method
    attr_reader :meth_name
    def initialize(meth_name, options={})
      @meth_name = meth_name
      @accepts = options[:accepts] || []
      @returns = options[:returns] || []

      # If the last argument is a Block, handle it as a special case.  We
      # do this to ensure that there's no conflict with any real arguments
      # which may accept Procs.
      @block = @accepts.pop if @accepts.last == Block

      if @accepts.find_all {|o| o.is_a? Array}.length > 1
        raise ContractError, "Cannot define more than one expected variable argument"
      end
    end

    def expects_block?
      not @block.nil?
    end

    def accepts_varargs?
      @accepts.last.is_a? Array
    end

    # Worst-case scenario: :accepts => [ Arg, Arg, [ VarArgs ], Block ]
    def check_accepts(*args, &block)
      @accepts.each_with_index do |expected_arg, i|
        # Varargs: consume all remaining arguments.
        if expected_arg.is_a? Array
          check_varargs(args, expected_arg.first, i) and break
        end
        check_argument(args[i], expected_arg)
      end
      if expects_block?
        check_argument(block, @block)
      end
      true
    end

    def check_varargs(given_args, expected, index)
      given_args[index..-1].each {|arg| check_argument(arg, expected)}
      true
    end

    def check_returns(*args)
      @returns.each_with_index do |expected, i|
        check_argument(args[i], expected)
      end
      true
    end

    # Raises an exception unless expected === given.
    def check_argument(given, expected)
      unless expected === given
        raise ContractViolation, "Contract violated in call to method " +
          "#{@meth_name}; expected #{expected.inspect}, received #{given.inspect}"
      end
    end
  end

  # For block-checking, we need a class which is_a? Proc for instance checking
  # purposes but isn't the same so as not to prevent the user from passing in
  # explicitly defined procs as arguments.
  class Block
    def Block.===(o); Proc === o; end
  end

  # Transforms the given block into a contract clause.  Clause fails if
  # the given block returns false or nil, passes otherwise.
  class Clause
    def initialize(mesg=nil, &block)
      @mesg = mesg
      @block = block
    end
    def ===(o)
      @block.call(o)
    end
    def inspect; @mesg || "block to pass"; end
    def ==(other)
      other.class == self.class && other.mesg == @mesg && other.block == @block
    end
  end

  # A collection of methods for defining constraints on method arguments.
  module ClauseMethods
    # Passes if the given block returns true when passed the argument.
    def assert(mesg=nil, &block)
      Clause.new(mesg, &block)
    end
    
    # Passes if the subclause does not pass on the argument.
    def not?(clause)
      assert("not #{clause.inspect}") { |o| not ( clause === o ) }
    end
    
    # Always passes.
    def any
      Clause.new { true }
    end

    # Passes if argument is true or false.
    def boolean?
      assert("true or false") { |o| ( o == true ) || ( o == false ) }
    end

    # Passes if any of the subclauses pass on the argument.
    def any?(*clauses)
      assert("any of #{clauses.inspect}") { |o| clauses.any? {|c| c === o} }
    end
    alias :or? :any?
    
    # Passes only if all of the subclauses pass on the argument.
    def all?(*clauses)
      assert("all of #{clauses.inspect}") { |o| clauses.all? {|c| c === o} }
    end
    alias :and? :all?
    
    # Passes if argument is numeric and nonzero.
    def nonzero?
      all? Numeric, assert("nonzero") {|o| o != 0}
    end

    # Passes if argument is Enumerable and the subclause passes on all of 
    # its objects.
    def many?(clause)
      many_with_map?(clause) { |o| o }
    end
    
    # Passes if argument is Enumerable and the subclause passes on all of
    # its objects, mapped over the given block.
    def many_with_map?(clause, mesg=nil, &block)
      map_mesg = ( mesg.nil? ? "" : " after map #{mesg}" )
      many_with_map = assert("many of #{clause.inspect}#{map_mesg}") do |o|
        o.map(&block).all? { |p| clause === p }
      end
      all? Enumerable, many_with_map
    end
    
    # Passes if argument is a Hash and if the key and value clauses pass all
    # of its keys and values, respectively.
    def hash_of?(key_clause, value_clause)
      all_keys   = many_with_map?(key_clause, "all keys")     { |kv| kv[0] }
      all_values = many_with_map?(value_clause, "all values") { |kv| kv[1] }
      all? Hash, all_keys, all_values
    end

    # Passes only if argument is a hash and does not contain any keys except
    # those given.
    def hash_with_keys(*keys)
      key_assertion = assert("contains keys #{keys.inspect}") do |o|
        ( o.keys - keys ).empty?
      end
      all? Hash, key_assertion
    end
    alias :hash_with_options :hash_with_keys

    # Passes if:
    # - argument is a hash, and
    # - argument contains only the keys explicitly specified in the given
    #   hash, and
    # - every value contract in the given hash passes every applicable value
    #   in the argument hash
    def hash_contract(hash)
      value_assertions = hash.keys.map do |k|
        assert("key #{k} requires #{hash[k].inspect}") do |o|
          o.has_key?(k) ? hash[k] === o[k] : true
        end
      end
      all? hash_with_keys(*hash.keys), *value_assertions
    end
    alias :hash :hash_contract

    # Passes if argument responds to all of the given methods.
    def responds_to?(*methods)
      respond_assertions = methods.map do |m| 
        assert("responds to #{m}") { |o| o.respond_to? m }
      end
      all? *respond_assertions
    end
  end
end

class ContractViolation < RuntimeError
end

class ContractError < RuntimeError
end
