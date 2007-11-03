Dir[File.join(File.dirname(__FILE__), 'handshake/**/*.rb')].sort.each { |lib| require lib }

require 'test/unit/assertions'

# A module for defining class and method contracts (as in design-by-contract
# programming).  To use it in your code, include this module in a class.
# Note that when you do so, that class's +new+ method will be replaced with
# one that returns a contract-checked proxy object.
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

  # Suppress Handshake contract checking, for use in production code.
  def Handshake.suppress!
    @suppress_handshake = true
  end

  def Handshake.suppressed?
    @suppress_handshake = false unless defined?(@suppress_handshake)
    @suppress_handshake
  end

  # When Handshake is included in a class, that class's +new+ method is
  # overridden to provide custom functionality.  A proxy object, returned
  # in place of the real object, filters all external method calls through
  # any contracts that have been defined.
  # <b>N.B.:<b> Handshake is designed to act as a barrier between an object and
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
      alias :__new__ :new
      # Override the class-level new method of every class that includes
      # Contract and cause it to return a proxy object for the original.
      def new(*args, &block)
        return __new__(*args, &block) if Handshake.suppressed?
        
        unless instantiable?
          raise ContractViolation, "This class has been marked as abstract and cannot be instantiated."
        end

        Handshake.catch_contract("Contract violated in call to constructor of class #{self}") do
          if contract_defined? :initialize
            method_contracts[:initialize].check_accepts!(*args, &block)
          end
        end

        o = __new__(*args, &block)
        raise ContractError, "Could not instantiate object" if o.nil?

        Handshake.catch_contract("Invariant violated by constructor of class #{self}") do
          o.check_invariants!
        end

        Proxy.new o
      end
    end
  end

  # This module contains methods that are mixed into any class that includes
  # Handshake.  They allow you to define constraints on that class and its
  # methods.  Subclasses will inherit the contracts and invariants of its
  # superclass, but Handshake contracts currently can't be mixed-in via a
  # module.
  #
  # This module defines three kinds of contracts: class invariants, method
  # signature constraints, and more general method pre- and post-conditions.
  # Invariants accept a block which should return a boolean.  Pre- and post-
  # conditions expect you to use assertions (all of Test::Unit's standard
  # assertions are available) and will pass unless an assertion fails.
  # Method signature contracts map inputs clauses to output clauses.  A
  # "clause" is defined as any object that implements the === method.
  #
  # All method contracts are defined on the method defined immediately after
  # their declaration unless a method name is specified.  For example,
  #
  #   contract :foo, String => Integer
  #
  # is equivalent to
  #
  #   contract String => Integer
  #   def foo ...
  #
  # ===Method signature contracts
  #   contract String => Integer
  #   contract [ String, 1..5, [ Integer ], Block ] => [ String, String ]
  #   contract clause("must equal 'foo'") { |o| o == "foo" } => anything
  #   contract Block(String => Integer) => Symbol
  #
  # A method signature contract is defined as a mapping from valid inputs to 
  # to valid outputs.  A clause here is any object which implements the
  # <tt>===</tt> method.  Classes, ranges, regexes, and other useful objects
  # are thus all valid values for a method signature contract.
  #
  # Multiple arguments are specified as an array.  To specify that a method
  # accepts varargs, define a nested array as the last or second-to-last
  # item in the array.  To specify that a method accepts a block, use the
  # Block clause, which accepts a method signature hash as an argument,
  # as above.  To specify that a method should accept a block, but that the
  # block should be unchecked, simply use Block instead of Block(... => ...).
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
      @instantiable = false
    end

    def instantiable?
      @instantiable ||= true
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
      attr_reader(*(meth_to_clause.keys))
      meth_to_clause.each do |meth, cls|
        contract meth, nil => cls
      end
    end

    # Defines contract-checked attribute writers with the given hash of method
    # name to clause.
    def contract_writer(meth_to_clause)
      attr_writer(*(meth_to_clause.keys))
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
      contract = contract_for(method).dup
      contract.signature = contract_hash
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
    
    protected
    # Returns the contract-checked proxy of self.
    def checked_self
      @checked_self || self
    end
  end

  # A ProcContract encapsulates knowledge about the signature of a method and
  # can check arrays of values against the signature through the
  # +check_equivalence!+ method.
  class ProcContract # :nodoc:
    attr_accessor :accepts, :returns

    def initialize
      @accepts, @returns = [], []
    end

    # Accepts signatures of the form:
    #   Clause => Clause
    #   [ Clause, Clause ] => Clause
    def signature=(contract_hash)
      raise ArgumentError unless contract_hash.length == 1
      sig_accepts, sig_returns = [ contract_hash.keys.first, contract_hash.values.first ].map {|v| arrayify v}
      self.accepts = sig_accepts
      self.returns = sig_returns
    end

    def check_accepts!(*args, &block)
      @accepts.each_with_index do |expected_arg, i|
        # Varargs: consume all remaining arguments.
        if expected_arg.is_a? Array
          check_varargs!(args, expected_arg.first, i) and break
        end
        check_equivalence!(args[i], expected_arg)
      end
    end

    def check_returns!(*args)
      @returns.each_with_index do |expected, i|
        check_equivalence!(args[i], expected)
      end
    end

    def accepts_varargs?
      accepts.last.is_a? Array
    end

    private
    def check_varargs!(given_args, expected, index)
      given_args[index..-1].each {|arg| check_equivalence!(arg, expected)}
    end

    def arrayify(value_or_array)
      value_or_array.is_a?(Array) ? value_or_array : [ value_or_array ]
    end

    
    # Checks the given value against the expected value using === and throws
    # :contract if it fails.  This is a bit clunky.
    def check_equivalence!(given, expected)
      unless expected === given
        mesg = "expected #{expected.inspect}, received #{given.inspect}"
        throw :contract, ContractViolation.new(mesg)
      end
    end
  end

  # Class representing method contracts.  Not for external use.
  class MethodContract < ProcContract # :nodoc: 
    attr_accessor :preconditions, :postconditions
    attr_reader :block_contract

    def initialize(method_name)
      @method_name = method_name
      @preconditions, @postconditions = [], []
      @accepts, @returns = [], []
      @block_contract = nil
    end

    def check_accepts!(*args, &block)
      super(*args, &block)
      if expects_block?
        check_equivalence!(block, Proc)
      end
    end

    # Returns true only if this MethodContract has been set up to check
    # for one or more contract conditions.
    def defined?
      [ preconditions, postconditions, accepts, returns ].any? do |ary|
        not ary.empty?
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

    # Checks the given conditions against the object, passing the given args
    # into the block.  Throws :contract if any fail or if an exception is
    # raised.  Because of the need to evaluate the condition in the context
    # of the object itself, a temporary method called +bound_condition_passes?+
    # is defined on the object, using the block associated with the condition.
    # TODO Is there a better way to evaluate an arbitary block in a particular binding?  There must be.
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

    # If the last argument is a Block, handle it as a special case.  We
    # do this to ensure that there's no conflict with any real arguments
    # which may accept Procs.
    def accepts=(args)
      if args.last == Block # Transform into a ProcContract
        args.pop
        @block_contract = ProcContract.new
        @block_contract.accepts = ClauseMethods::ANYTHING
        @block_contract.returns = ClauseMethods::ANYTHING        
      elsif args.last.is_a?(ProcContract)
        @block_contract = args.pop
      end

      if args.find_all {|o| o.is_a? Array}.length > 1
        raise ContractError, "Cannot define more than one expected variable argument"
      end
      super(args)
    end

    def expects_block?
      not @block_contract.nil?
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
      o.instance_eval(&block)
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
    proxy_self(*SELF_PROXIED)

    # Accepts an object to be proxied.
    def initialize(proxied)
      @proxied = proxied
      @proxied.instance_variable_set(:@checked_self, self)
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
        contract.check_accepts!(*args, &block)
        contract.check_pre! @proxied, *args
      end
        
      # make actual call, wrapping the given block in a new block so that
      # contract checks work if receiver uses yield.
      return_val = nil
      if contract.expects_block?
        cp = CheckedProc.new(contract.block_contract, &block)
        return_val = @proxied.send(meth_name, *args) { |*argz| cp.call(*argz) }
      else
        return_val = @proxied.send(meth_name, *args, &block)
      end
         
      Handshake.catch_contract("Contract violated by #{meth_string}") do
        contract.check_returns! return_val
        contract.check_post! @proxied, *(args << return_val)
        @proxied.check_invariants!
      end

      return return_val
    end
    alias :method_missing :send

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
