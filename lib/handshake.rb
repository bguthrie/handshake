# handshake.rb
# Copyright (c) 2007 Brian Guthrie
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

require 'inheritable_attributes'
require 'test/unit/assertions'

class Class # :nodoc:
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
# programming).  To use it in your code, include this module in a class.
# Note that when you do so, that class's +new+ method will be replaced.
# There are three different types of contracts you can specify on a class.
# See Handshake::ClassMethods for more documentation.
module Handshake

  # Catches any thrown :contract exception raised within the given block, 
  # appends the given message to the violation message, and re-raises the 
  # exception.
  def Handshake.catch_contract(mesg=nil, &block) # :nodoc:
    violation = catch(:contract, &block)
    if violation.is_a?(Exception)
      # Re-raise the violation with the given message, ensuring that the
      # callback stack begins with the caller of this method rather than
      # this method.
      message = ( mesg.nil? ? "" : ( mesg + ": " ) ) + violation.message
      raise violation.class, message, caller
    end
  end

  # When Handshake is included in a class, that class's +new+ method is
  # overridden to provide custom functionality.  A proxy object, returned
  # in place of the real object, filters all external method calls through
  # any contracts that have been defined.
  # *N.B.:* Handshake is designed to act as a barrier between an object and
  # its callers.  However, anything that takes place within that barrier
  # is not checked.  This means that Handshake is, at the moment, unable
  # to enforce contracts on methods called only internally, notably private
  # methods.
  def Handshake.included(base)
    base.extend(ClassMethods)
    base.extend(ClauseMethods)

    base.send(:include, Test::Unit::Assertions)
    base.send(:include, Handshake::InstanceMethods)

    base.class_inheritable_array :invariants
    base.write_inheritable_array :invariants, []

    base.class_inheritable_hash :method_contracts
    base.write_inheritable_hash :method_contracts, {}

    class << base
      alias :instantiate :new
      # Override the class-level new method of every class that includes
      # Contract and cause it to return a proxy object for the original.
      def new(*args, &block)
        if @non_instantiable
          raise ContractViolation, "This class has been marked as abstract and cannot be instantiated."
        end
        o = nil

        # Special case:  check invariants for constructor.
        Handshake.catch_contract("Contract violated in call to constructor of class #{self}") do
          if contract_defined? :initialize
            method_contracts[:initialize].check_accepts!(*args, &block)
          end
        end

        o = self.instantiate(*args, &block)

        Handshake.catch_contract("Invariant violated by constructor of class #{self}") do
          o.check_invariants!
        end

        raise ContractError, "Could not instantiate object" if o.nil?
        Proxy.new( o )
      end
    end
  end

  # Defines class methods for contracts.  All methods accept an optional method
  # name to assign the contract to as their first argument.  If none is given, 
  # contract will be assigned to the next added method.  Contracts defined on
  # superclasses are inherited by its subclasses.  Subclasses may add new
  # invariants and override other existing contracts by redefining them.
  #
  # ===Method signature contracts
  #   contract String => Integer
  #   contract [ String, 1..5, [ Integer ], Block ] => [ String, String ]
  #   contract clause("must equal 'foo'") { |o| o == "foo" } => anything
  #
  # A method signature contract is defined as a mapping from valid inputs to 
  # to valid outputs.  A clause here is any object which implements the
  # <tt>===</tt> method.  Classes, ranges, regexes, and other useful objects
  # are thus all valid values for a method signature contract.
  #
  # Multiple arguments are specified as an array.  To specify that a method
  # accepts varargs, define a nested array as the last or second-to-last
  # item in the array.  To specify that a method accepts a block, place
  # the Block constant as the last item in the array.  Expect this to change
  # in the future to allow for block contracts.
  #
  # New clauses may be created easily with the Handshake::ClauseMethods#clause
  # method.  Handshake::ClauseMethods also provides a number of useful contract
  # combinators for specifying rich input and output contracts.
  #
  # ===Contract-checked accessors
  #   contract_reader :foo => String, :bar => Integer
  #   contract_writer ...
  #   contract_accessor ...
  # Defines contract-checked accessors.  Method names and clauses are specified
  # in a hash.  Hash values are any valid clause.
  #
  # ===Invariants
  #   invariant(optional_message) { returns true }
  # Aliased as +always+.  Has access to instance variables and methods of object
  # but calls to same are unchecked.
  #
  # ===Pre/post-conditions
  #   before(optional_message) { |arg1, ...| assert condition }
  #   after(optional_message)  { |arg1, ..., returned| assert condition }
  #   around(optional_message) { |arg1, ...| assert condition }
  # Check a set of conditions, using assertions, before and after method
  # invocation.  +before+ and +after+ are aliased as +requires+ and +ensures+ 
  # respectively.  +around+ currently throws a block argument warning; this
  # should be fixed soon.  Same scope rules as invariants, so you can check
  # instance variables and local methods.  All Test::Unit::Assertions are available
  # for use, but any such AssertionFailed errors encountered are re-raised 
  # by Handshake as Handshake::AssertionFailed errors to avoid confusion
  # with test case execution.
  #
  # ===Abstract class decorator
  #   class SuperDuperContract
  #     include Handshake; abstract!
  #     ...
  #   end
  #
  # To define a class as non-instantiable and have Handshake raise a
  # ContractViolation if a caller attempts to do so, call <tt>abstract!</tt>
  # at the top of the class definition.  This attribute is not inherited
  # by subclasses, but is useful if you would like to define a pure-contract
  # superclass that isn't intended to be instantiated directly.
  module ClassMethods
    # Define this class as non-instantiable.  Subclasses do not inherit this
    # attribute.
    def abstract!
      @non_instantiable = true
    end

    # Specify an invariant, with a block and an optional error message.
    def invariant(mesg=nil, &block) # :yields:
      write_inheritable_array(:invariants, [ Invariant.new(mesg, &block) ] )
      nil
    end
    alias :always :invariant

    # In order for contract clauses to work in conjunction with Handshake
    # proxy objects, the === method must be redefined in terms of is_a?.
    def ===(other)
      other.is_a? self
    end
    
    # Specify an argument contract, with argument clauses on one side of the
    # hash arrow and returned values on the other.  Each clause must implement
    # the === method or have been created with the assert method.  This
    # method should generally not be called directly.
    def contract(meth_or_hash, contract_hash=nil)
      if meth_or_hash.is_a? Hash
        defer :contract, meth_or_hash
      else
        define_contract(meth_or_hash, contract_hash)
      end
    end

    # Specify a precondition.
    def before(meth_or_mesg=nil, mesg=nil, &block)
      condition(:before, meth_or_mesg, mesg, &block)
    end
    alias :requires :before

    # Specify a postcondition.
    def after(meth_or_mesg=nil, mesg=nil, &block)
      condition(:after, meth_or_mesg, mesg, &block)
    end
    alias :ensures :after

    # Specify a bothcondition.
    def around(meth_or_mesg=nil, mesg=nil, &block)
      condition(:around, meth_or_mesg, mesg, &block)
    end

    # Returns the MethodContract for the given method name. Side effect:
    # creates one if none defined.
    def contract_for(method)
      if contract_defined?(method)
        method_contracts[method]
      else
        contract = MethodContract.new("#{self}##{method}")
        write_inheritable_hash :method_contracts, { method => contract }
        contract
      end
    end

    # Returns true if a contract is defined for the named method.
    def contract_defined?(method)
      method_contracts.has_key?(method)
    end

    # Defines contract-checked attribute readers with the given hash of method
    # name to clause.
    def contract_reader(meth_to_clause)
      attr_reader *(meth_to_clause.keys)
      meth_to_clause.each do |meth, cls|
        contract meth, nil => cls
      end
    end

    # Defines contract-checked attribute writers with the given hash of method
    # name to clause.
    def contract_writer(meth_to_clause)
      attr_writer *(meth_to_clause.keys)
      meth_to_clause.each do |meth, cls|
        contract "#{meth}=".to_sym, cls => anything
      end
    end

    # Defines contract-checked attribute accessors for the given hash of method
    # name to clause.
    def contract_accessor(meth_to_clause)
      contract_reader meth_to_clause
      contract_writer meth_to_clause
    end

    # Callback from method add event.  If a previous method contract
    # declaration was deferred, complete it now with the name of the newly-
    # added method.
    def method_added(meth_name)
      @deferred ||= {}
      unless @deferred.empty?
        @deferred.each do |k, v|
          case k
          when :before, :after, :around
            define_condition meth_name, k, v
          when :contract
            define_contract meth_name, v
          end
        end
        @deferred.clear
      end
    end

    private

    def define_contract(method, contract_hash)
      raise ArgumentError unless contract_hash.length == 1
      accepts, returns = [ contract_hash.keys.first, contract_hash.values.first ].map {|v| arrayify v}
      contract = contract_for(method).dup
      contract.accepts = accepts
      contract.returns = returns
      write_inheritable_hash :method_contracts, { method => contract }
    end

    def define_condition(method, type, condition)
      defined_before = [ :before, :around ].include? type
      defined_after  = [ :after,  :around ].include? type
      contract = contract_for(method).dup
      contract.preconditions << condition if defined_before
      contract.postconditions << condition if defined_after
      write_inheritable_hash :method_contracts, { method => contract }
    end

    def condition(type, meth_or_mesg=nil, mesg=nil, &block)
      method_specified = meth_or_mesg.is_a?(Symbol)
      message = method_specified ? mesg : meth_or_mesg
      condition = MethodCondition.new(message, &block)
      if method_specified
        define_condition(type, meth_or_mesg, condition)
      else
        defer type, condition
      end
    end

    def arrayify(value_or_array)
      value_or_array.is_a?(Array) ? value_or_array : [ value_or_array ]
    end

    def defer(type, value)
      ( @deferred ||= {} )[type] = value
    end

  end

  module InstanceMethods
    # Checks the invariants defined on this class against +self+, raising a
    # ContractViolation if any of them fail.
    def check_invariants!
      self.class.invariants.each do |invar|
        unless invar.holds?(self)
          mesg = invar.mesg || "Invariant check failed"
          throw :contract, ContractViolation.new(mesg)
        end
      end
    end
  end

  # Class representing method contracts.  Not for external use.
  class MethodContract # :nodoc: 
    attr_accessor :preconditions, :postconditions, :returns
    attr_reader :accepts

    def initialize(method_name)
      @method_name = method_name
      @preconditions, @postconditions, @accepts, @returns = [], [], [], []
    end

    # Returns true only if this MethodContract has been set up to check
    # for one or more contract conditions.
    def defined?
      [ @preconditions, @postconditions, @accepts, @returns ].all? do |ary|
        ary.empty?
      end
    end

    # Checks the postconditions of this contract against the given object
    # and return values.  Any assertions thrown are re-raised as
    # Handshake::AssertionViolation errors.
    def check_post!(o, *args)
      check_conditions!(o, args, @postconditions)
    end

    # Checks the preconditions of this contract against the given object
    # and arugment values.  Any assertions thrown are re-raised as
    # Handshake::AssertionFailed errors.
    def check_pre!(o, *args)
      check_conditions!(o, args, @preconditions)
    end

    def check_conditions!(o, args, conditions)
      conditions.each do |condition|
        o.class.instance_eval do
          define_method(:bound_condition_passes?, &(condition.block))
        end
        begin
          o.bound_condition_passes?(*args)
        rescue Test::Unit::AssertionFailedError => afe
          throw :contract, AssertionFailed.new(afe.message)
        rescue Exception => e
          throw :contract, e
        end
        o.class.send(:remove_method, :bound_condition_passes?)
      end
    end

    def accepts=(args)
      # If the last argument is a Block, handle it as a special case.  We
      # do this to ensure that there's no conflict with any real arguments
      # which may accept Procs.
      @block = args.pop if args.last == Block

      if args.find_all {|o| o.is_a? Array}.length > 1
        raise ContractError, "Cannot define more than one expected variable argument"
      end
      @accepts = args
    end

    def expects_block?
      not @block.nil?
    end

    def accepts_varargs?
      @accepts.last.is_a? Array
    end

    def check_accepts!(*args, &block)
      @accepts.each_with_index do |expected_arg, i|
        # Varargs: consume all remaining arguments.
        if expected_arg.is_a? Array
          check_varargs!(args, expected_arg.first, i) and break
        end
        check_equivalence!(args[i], expected_arg)
      end
      if expects_block?
        check_equivalence!(block, @block)
      end
    end

    def check_returns!(*args)
      @returns.each_with_index do |expected, i|
        check_equivalence!(args[i], expected)
      end
    end

    def check_varargs!(given_args, expected, index)
      given_args[index..-1].each {|arg| check_equivalence!(arg, expected)}
    end

    def check_equivalence!(given, expected)
      unless expected === given
        mesg = "expected #{expected.inspect}, received #{given.inspect}"
        throw :contract, ContractViolation.new(mesg)
      end
    end
  end

  # Specifies a condition on a method.  Not for external use.
  class MethodCondition # :nodoc:
    attr_accessor :message, :block
    def initialize(message=nil, &block)
      @message, @block = message, block
    end
  end

  # This class defines a class invariant, which has a block and an optional
  # method.  Not for external use.
  class Invariant # :nodoc: 
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

  # This class filters all method calls to its proxied object through any
  # contracts defined on that object's class.  It attempts to look and act
  # like its proxied object for all intents and purposes, although it notably
  # does not proxy +__id__+, +__send__+, or +class+.
  class Proxy
    NOT_PROXIED  = [ "__id__", "__send__" ]
    SELF_PROXIED = Object.instance_methods - NOT_PROXIED

    # Redefine language-level methods inherited from Object, ensuring that
    # they are forwarded to the proxy object.
    proxy_self *SELF_PROXIED

    # Accepts an object to be proxied.
    def initialize(proxied)
      @proxied = proxied
    end

    # Returns the wrapped object.  Method calls made against this object
    # will not be checked.
    def unchecked!
      @proxied
    end

    # Returns the class of the proxied object.
    def proxied_class
      @proxied.class
    end

    # Override the send method, and alias method_missing to same.
    # This method intercepts all method calls and runs them through the
    # contract filter.  The order of contract checks is as follows:
    # * Before: invariants, method signature, precondition
    # * Method is called
    # * After: method signature, postcondition, invariants
    def send(meth_name, *args, &block)
      meth_string = "#{@proxied.class}##{meth_name}"
      contract = @proxied.class.contract_for(meth_name)
      return_val = nil
      # Use throw/catch rather than raise/rescue in order to pull exceptions
      # once and only once from within the stack trace.
      Handshake.catch_contract("Contract violated in call to #{meth_string}") do
        @proxied.check_invariants!
        contract.check_accepts! *args, &block
        contract.check_pre! @proxied, *args
      end
        
      # make actual call
      return_val = @proxied.send meth_name, *args, &block
        
      Handshake.catch_contract("Contract violated by #{meth_string}") do
        contract.check_returns! return_val
        contract.check_post! @proxied, *(args << return_val)
        @proxied.check_invariants!
      end

      return return_val
    end
    alias :method_missing :send


  end

  # For block-checking, we need a class which is_a? Proc for instance checking
  # purposes but isn't the same so as not to prevent the user from passing in
  # explicitly defined procs as arguments.  Expect this to be replaced at
  # some point in the future with a +block_contract+ construct.
  class Block
    def Block.===(o); Proc === o; end
  end

  # Transforms the given block into a contract clause.  Clause fails if
  # the given block returns false or nil, passes otherwise.  See
  # Handshake::ClauseMethods for more examples of its use.  This object may
  # be instantiated directly but calling Handshake::ClauseMethods#clause is
  # generally preferable.
  class Clause
    # Defines a new Clause object with a block and a message.
    # The block should return a boolean value.  The message is optional but
    # strongly recommended for human-readable contract violation errors.
    def initialize(mesg=nil, &block) # :yields: argument
      @mesg, @block = mesg, block
    end
    # Returns true if the block passed to the constructor returns true when
    # called with the given argument.
    def ===(o)
      @block.call(o)
    end
    # Returns the message defined for this Clause, or "undocumented clause"
    # if none is defined.
    def inspect; @mesg || "undocumented clause"; end
    def ==(other)
      other.class == self.class && other.mesg == @mesg && other.block == @block
    end
  end

  # A collection of methods for defining constraints on method arguments.
  module ClauseMethods
    # Passes if the given block returns true when passed the argument.
    def clause(mesg=nil, &block) # :yields: argument
      Clause.new(mesg, &block)
    end
    
    # Passes if the subclause does not pass on the argument.
    def not?(clause)
      clause("not #{clause.inspect}") { |o| not ( clause === o ) }
    end
    
    # Always passes.
    def anything
      Clause.new("anything") { true }
    end

    # Passes if argument is true or false.
    #   contract self => boolean?
    #   def ==(other)
    #     ...
    #   end
    def boolean?
      #clause("true or false") { |o| ( o == true ) || ( o == false ) }
      any?(TrueClass, FalseClass)
    end

    # Passes if any of the subclauses pass on the argument.
    #   contract any?(String, Symbol) => anything
    def any?(*clauses)
      clause("any of #{clauses.inspect}") { |o| clauses.any? {|c| c === o} }
    end
    alias :or? :any?
    
    # Passes only if all of the subclauses pass on the argument.
    #   contract all?(Integer, nonzero?)
    def all?(*clauses)
      clause("all of #{clauses.inspect}") { |o| clauses.all? {|c| c === o} }
    end
    alias :and? :all?
    
    # Passes if argument is numeric and nonzero.
    def nonzero?
      all? Numeric, clause("nonzero") {|o| o != 0}
    end

    # Passes if argument is Enumerable and the subclause passes on all of 
    # its objects.
    #
    #   class StringArray < Array
    #     include Handshake
    #     contract :+, many?(String) => self
    #   end
    def many?(clause)
      many_with_map?(clause) { |o| o }
    end
    
    # Passes if argument is Enumerable and the subclause passes on all of
    # its objects, mapped over the given block.
    #   contract many_with_map?(nonzero?, "person age") { |person| person.age } => anything
    def many_with_map?(clause, mesg=nil, &block) # :yields: argument
      map_mesg = ( mesg.nil? ? "" : " after map #{mesg}" )
      many_with_map = clause("many of #{clause.inspect}#{map_mesg}") do |o|
        o.map(&block).all? { |p| clause === p }
      end
      all? Enumerable, many_with_map
    end
    
    # Passes if argument is a Hash and if the key and value clauses pass all
    # of its keys and values, respectively.
    # E.g. <tt>hash_of?(Symbol, String)</tt>:
    #   
    #   :foo => "bar", :baz => "qux" # passes
    #   :foo => "bar", "baz" => 3    # fails
    def hash_of?(key_clause, value_clause)
      all_keys   = many_with_map?(key_clause, "all keys")     { |kv| kv[0] }
      all_values = many_with_map?(value_clause, "all values") { |kv| kv[1] }
      all? Hash, all_keys, all_values
    end

    # Passes only if argument is a hash and does not contain any keys except
    # those given.
    # E.g. <tt>hash_with_keys(:foo, :bar, :baz)</tt>:
    # 
    #   :foo => 3                                     # passes
    #   :foo => 10, :bar => "foo"                     # passes
    #   :foo => "eight", :chunky_bacon => "delicious" # fails
    def hash_with_keys(*keys)
      key_assertion = clause("contains keys #{keys.inspect}") do |o|
        ( o.keys - keys ).empty?
      end
      all? Hash, key_assertion
    end
    alias :hash_with_options :hash_with_keys

    # Passes if:
    # * argument is a hash, and
    # * argument contains only the keys explicitly specified in the given
    #   hash, and
    # * every value contract in the given hash passes every applicable value
    #   in the argument hash
    # E.g. <tt>hash_contract(:foo => String, :bar => Integer)</tt>
    #
    #   :foo => "foo"               # passes
    #   :bar => 3                   # passes
    #   :foo => "bar", :bar => 42   # passes
    #   :foo => 88, :bar => "none"  # fails
    def hash_contract(hash)
      value_assertions = hash.keys.map do |k|
        clause("key #{k} requires #{hash[k].inspect}") do |o|
          o.has_key?(k) ? hash[k] === o[k] : true
        end
      end
      all? hash_with_keys(*hash.keys), *value_assertions
    end

    # Passes if argument responds to all of the given methods.
    def responds_to?(*methods)
      respond_assertions = methods.map do |m| 
        clause("responds to #{m}") { |o| o.respond_to? m }
      end
      all? *respond_assertions
    end
    
    # Allows you to check whether the argument is_a? of the given symbol.
    # For example, is?(:String).  Useful for situations where you want
    # to check for a class type that hasn't been defined yet when Ruby
    # evaluates the contract but will have been by the time the code runs.
    # Note that <tt>String => anything</tt> is equivalent to
    # <tt>is?(:String) => anything</tt>.
    def is?(class_symbol)
      clause(class_symbol.to_s) { |o|
        Object.const_defined?(class_symbol) && o.is_a?(Object.const_get(class_symbol))
      }
    end

  end

  class ContractViolation < RuntimeError; end
  class AssertionFailed   < ContractViolation; end
  class ContractError     < RuntimeError; end
end


module Test # :nodoc:
  module Unit # :nodoc:
    module Assertions
      # Asserts that the given block violates the contract by raising an
      # instance of Handshake::ContractViolation.
      def assert_violation(&block)
        assert_raise(Handshake::ContractViolation, Handshake::AssertionFailed, &block)
      end

      def assert_passes(&block)
        assert_nothing_raised(&block)
      end
    end
  end
end
