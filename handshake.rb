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
module Handshake
  def Handshake.included(base)
    base.extend(ClassMethods)
    base.extend(ClauseMethods)
    #base.extend(Test::Unit::Assertions)
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
          raise ContractError, "This class has been marked as abstract and cannot be instantiated."
        end
        o = nil

        violation = catch :contract do
          # Special case:  check invariants for constructor.
          if contract_defined? :initialize
            method_contracts[:initialize].check_accepts!(*args, &block)
          end

          o = self.instantiate(*args, &block)
          o.check_invariants!
        end
        raise violation if violation.is_a?(Exception)
        raise ContractError, "Could not instantiate object" if o.nil?
        Proxy.new( o )
      end
    end
  end

  # Defines class methods for contracts.  All methods accept an optional method
  # name to assign the contract to as their first argument.  If none is given, 
  # contract will be assigned to the next added method.
  #
  # Argument contracts:
  #   contract [ accepted, arguments ] => [ returned, arguments ]
  # Varargs specified with nested lists.  If single argument or return value,
  # brackets may be omitted.  Block specified as Block, final argument only.
  # Don't yet support block/proc contracts.
  #
  # Contract-checked accessors:
  #   contract_reader :foo => String, :bar => Integer
  #   contract_writer ...
  #   contract_accessor ...
  # Defines contract-checked accessors.  Method names and clauses are specified
  # in a hash.
  #
  # Invariants: 
  #   invariant(optional_message) { returns true }
  # Aliased as 'always'.  Has access to instance variables and methods of class
  # but calls to same are unchecked.
  #
  # Pre/post-conditions:
  #   before(optional_message) { |all, args| returns true }
  #   after(optional_message)  { |all, args, returned| returns true }
  #   around(optional_message) { |all, args| returns true }
  # Before and after aliased as 'requires' and 'ensure' respectively.  Around
  # currently throws a warning post-invocation.  Same scope rules as
  # invariants.
  module ClassMethods
    # Define this class as non-instantiable.  Subclasses do not inherit this
    # attribute.
    def abstract!
      @non_instantiable = true
    end

    # Specify an invariant, with a block and an optional error message.
    def invariant(mesg=nil, &block)
      write_inheritable_array(:invariants, [ Invariant.new(mesg, &block) ] )
      nil
    end
    alias :always :invariant
    
    # Specify an argument contract, with argument clauses on one side of the
    # hash arrow and returned values on the other.  Each clause must implement
    # the === method or have been created with the assert method.
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
        contract = MethodContract.new(method)
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
          when :condition: define_condition meth_name, v.keys.first, v.values.first
          when :contract:  define_contract meth_name, v
          end
        end
        @deferred.clear
      end
    end

    private

    def define_contract(method, contract_hash)
      raise ArgumentError unless contract_hash.length == 1
      accepts, returns = [ contract_hash.keys.first, contract_hash.values.first ].map {|v| arrayify v}
      contract_for(method).accepts = accepts
      contract_for(method).returns = returns
    end

    def define_condition(method, type, condition)
      defined_before = [ :before, :around ].include? type
      defined_after  = [ :after,  :around ].include? type
      contract_for(method).preconditions << condition if defined_before
      contract_for(method).postconditions << condition if defined_after
    end

    def condition(type, meth_or_mesg=nil, mesg=nil, &block)
      method_specified = meth_or_mesg.is_a?(Symbol)
      message = method_specified ? mesg : meth_or_mesg
      condition = MethodCondition.new(message, &block)
      if method_specified
        define_condition(meth_or_mesg, condition)
      else
        defer :condition, { type => condition }
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
    # Checks the invariants of this object, raising a ContractViolation if any
    # fail.
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
  class MethodContract
    attr_accessor :preconditions, :postconditions, :returns
    attr_reader :accepts

    def initialize(method_name)
      @method_name = method_name
      @preconditions, @postconditions, @accepts, @returns = [], [], [], []
    end

    def defined?
      [ @preconditions, @postconditions, @accepts, @returns ].all? do |ary|
        ary.empty?
      end
    end

    def check_post!(o, *args)
      check_conditions!(o, args, @postconditions)
    end

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
      args = [ args ] unless args.is_a? Array
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
        exc = ContractViolation.new("Contract violated in call to method " +
          "#{@method_name}; expected #{expected.inspect}, received #{given.inspect}")
        throw :contract, exc
      end
    end
  end

  # Specifies a condition on a method.  Not for external use.
  class MethodCondition
    attr_accessor :message, :block
    def initialize(message=nil, &block)
      @message, @block = message, block
    end
  end

  # This class defines a class invariant, which has a block and an optional
  # method.  Not for external use.
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
      contract = @proxied.class.contract_for(meth_name)
      return_val = nil
      # Use throw/catch rather than raise/rescue in order to pull exceptions
      # once and only once from within the stack trace.
      violation = catch :contract do |exc|
        # before
        @proxied.check_invariants!
        contract.check_accepts! *args, &block
        contract.check_pre! @proxied, *args
        
        # make actual call
        return_val = @proxied.send meth_name, *args, &block
        return_val = [ return_val ] unless return_val.is_a? Array
        
        # after
        contract.check_returns! *(args + return_val)
        contract.check_post! @proxied, *(args + return_val)
        @proxied.check_invariants!
      end
      raise violation if violation.is_a?(Exception)
      return return_val
    end
    alias :method_missing :send

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
      @mesg, @block = mesg, block
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
    def clause(mesg=nil, &block)
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
    def boolean?
      clause("true or false") { |o| ( o == true ) || ( o == false ) }
    end

    # Passes if any of the subclauses pass on the argument.
    def any?(*clauses)
      clause("any of #{clauses.inspect}") { |o| clauses.any? {|c| c === o} }
    end
    alias :or? :any?
    
    # Passes only if all of the subclauses pass on the argument.
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
    def many?(clause)
      many_with_map?(clause) { |o| o }
    end
    
    # Passes if argument is Enumerable and the subclause passes on all of
    # its objects, mapped over the given block.
    def many_with_map?(clause, mesg=nil, &block)
      map_mesg = ( mesg.nil? ? "" : " after map #{mesg}" )
      many_with_map = clause("many of #{clause.inspect}#{map_mesg}") do |o|
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
      key_assertion = clause("contains keys #{keys.inspect}") do |o|
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
    # For example, is_a?(:String).  Useful for situations where you want
    # to check for a class type that hasn't been defined yet when you write
    # the contract but will have been by the time the code runs.
    def is_a?(class_symbol)
      clause("is a #{class_symbol}") { |o|
        Object.const_defined?(class_symbol) && o.is_a?(Object.const_get(class_symbol))
      }
    end
  end

  class ContractViolation < RuntimeError; end
  class AssertionFailed   < ContractViolation; end
  class ContractError     < RuntimeError; end
end


module Test
  module Unit
    module Assertions
      def assert_violation(&block)
        assert_raise(Handshake::ContractViolation, Handshake::AssertionFailed, &block)
      end
    
      def assert_passes(&block)
        assert_nothing_raised(&block)
      end
    end
  end
end

class Foo
  include Handshake
  contract String => anything
  def initialize(str); @str = str; end
end

