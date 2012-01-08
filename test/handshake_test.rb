require 'rubygems'
require 'shoulda'
require 'handshake'
require 'handshake/assertions'

class HandshakeTest < Test::Unit::TestCase
  include Handshake::Assertions

  context Handshake do
    context "invariant" do
      class InvariantDeclarations
        include Handshake
        invariant { true }
      end
    
      class ExtendsInvariantDeclarations < InvariantDeclarations
        invariant { true }
      end
    
      should "correctly track the list of invariants in superclasses and subclasses" do
        assert_equal 1, InvariantDeclarations.invariants.length
        assert_equal 2, ExtendsInvariantDeclarations.invariants.length
      end
    
      class NonFunctionalArray < Array
        include Handshake
        invariant { false }
      end
    
      should "fail a very simple invariant check" do
        assert_violation { NonFunctionalArray.new }
      end
      
      class PositiveBalance
        include Handshake
        invariant { @balance > 0 }
        attr_accessor :balance
        def initialize(balance); @balance = balance; end
      end
    
      should "check invariants that leverage new instance methods" do
        assert_violation { PositiveBalance.new(-10) }
        assert_violation { PositiveBalance.new 0 }
        assert_passes    { PositiveBalance.new 10 }
        assert_violation { 
          pb = PositiveBalance.new(10); pb.balance = -10
        }
      end
    
      class NonEmptyArray < Array
        include Handshake
        invariant { not empty? }
      end
      class ExtendsNonEmptyArray < NonEmptyArray; end
      
      should "check invariants that leverage inherited instance methods" do
        assert_violation { NonEmptyArray.new }
        assert_passes    { NonEmptyArray.new [1] }
        assert_violation { ExtendsNonEmptyArray.new }
        assert_passes    { ExtendsNonEmptyArray.new [1] }
    
        assert_violation { NonEmptyArray.new([1]).pop }
      end
    end
    
    context "contract" do
      class MethodDeclarations
        include Handshake
        contract :accepts_str, String => anything
        contract :accepts_int, Integer => anything
      end
      class ExtendsMethodDeclarations < MethodDeclarations; end
    
      should "tie contract declaration to the method name to which they apply, and apply them to subclasses" do
        assert MethodDeclarations.method_contracts.has_key?(:accepts_str)
        assert MethodDeclarations.method_contracts.has_key?(:accepts_int)
        assert ExtendsMethodDeclarations.method_contracts.has_key?(:accepts_str)
        assert ExtendsMethodDeclarations.method_contracts.has_key?(:accepts_int)
      end
      
      class AcceptsString
        include Handshake
        contract :initialize, String => anything
        def initialize(str); @str = str; end
        contract String => anything
        def str=(str); @str = str; end
      end
      class ExtendsAcceptsString < AcceptsString; end
      
      class AcceptsIntegerInstead < AcceptsString
        contract :initialize, Integer => anything
      end
      
      class AcceptsSymbolInstead < AcceptsString
        contract :initialize, Symbol => anything
      end
    
      should "check simple type contracts for a single method argument" do
        assert_violation { AcceptsString.new 3 }
        assert_violation { AcceptsString.new :foo }
        assert_passes    { AcceptsString.new "string" }
        assert_violation { AcceptsString.new("foo").str = 3 }
        assert_violation { ExtendsAcceptsString.new 3 }
        assert_violation { ExtendsAcceptsString.new :foo }
        assert_passes    { ExtendsAcceptsString.new "string" }
        assert_violation { ExtendsAcceptsString.new("foo").str = 3 }
        assert_violation { AcceptsIntegerInstead.new("foo") }
        assert_passes    { AcceptsIntegerInstead.new 3 }
        assert_violation { AcceptsSymbolInstead.new "foo" }
        assert_violation { AcceptsSymbolInstead.new 3 }
        assert_passes    { AcceptsSymbolInstead.new :foo }
      end
      
      class ReturnsString
        include Handshake
        contract anything => String
        def call(val); val; end
      end
      class ExtendsReturnsString < ReturnsString; end
    
      should "check simple type contracts for a single method return value" do
        assert_violation { ReturnsString.new.call(1) }
        assert_violation { ReturnsString.new.call(true) }
        assert_passes    { ReturnsString.new.call("foo") }
        assert_violation { ExtendsReturnsString.new.call(1) }
        assert_violation { ExtendsReturnsString.new.call(true) }
        assert_passes    { ExtendsReturnsString.new.call("foo") }
      end
      
      class ReturnsMultiple
        include Handshake
        contract [ String, Integer ] => anything
        def call(arg1, arg2); return arg1, arg2; end
      end
    
      should "check simple type contracts for multiple method arguments" do
        assert_violation { ReturnsMultiple.new.call("foo", "foo") }
        assert_violation { ReturnsMultiple.new.call(3, 3) }
        assert_passes    { ReturnsMultiple.new.call("foo", 3) }
      end
      
      class AcceptsVarargs
        include Handshake
        contract [[ String ]] => anything
        def initialize(*strs); @strs = strs; end
      end
    
      should "check simple type contracts for methods that accept varargs" do
        assert_passes    { AcceptsVarargs.new }
        assert_violation { AcceptsVarargs.new(1, 2, 3) }
        assert_violation { AcceptsVarargs.new("foo", 1, 2) }
        assert_violation { AcceptsVarargs.new(:foo, "foo") }
        assert_passes    { AcceptsVarargs.new("foo") }
        assert_passes    { AcceptsVarargs.new("foo1", "foo2") }
      end
      
      class AcceptsBlock
        include Handshake
        contract Block => anything
        def call1; end
        contract Block => anything
        def call2(&block); end
      end
    
      should "check simple type contracts that ensure methods can accept a block" do
        assert_violation { AcceptsBlock.new.call1 }
        assert_violation { AcceptsBlock.new.call2 }
        assert_passes    { AcceptsBlock.new.call1 { true } }
        assert_passes    { AcceptsBlock.new.call2 { true } }
        assert_passes    { AcceptsBlock.new.call1 { "foo" } }
        assert_violation { AcceptsBlock.new.call1("foo") }
        assert_violation { AcceptsBlock.new.call2("foo") }
      end
      
      class AcceptsWriter
        include Handshake
        contract String => anything
        def val=(str); @str = str; end
      end
    
      should "check simple type contracts for writer methods" do
        assert_violation { AcceptsWriter.new.val = 3 }
        assert_violation { AcceptsWriter.new.val = :foo }
        assert_passes    { AcceptsWriter.new.val = "foo" }
      end
      
      class AcceptsMixed
        include Handshake
        contract [ String, String, [ Integer ], Block ] => String
        def call(str1, str2, *ints, &block); "foo"; end
      end
    
      should "check simple type contracts for methods that accept multiple arguments, varargs, and a block" do
        assert_violation { AcceptsMixed.new.call }
        assert_violation { AcceptsMixed.new.call 3 }
        assert_violation { AcceptsMixed.new.call "foo" }
        assert_violation { AcceptsMixed.new.call "foo", 3 }
        assert_violation { AcceptsMixed.new.call "foo", "bar" }
        assert_passes    { AcceptsMixed.new.call("foo", "bar") { true } }
        assert_passes    { AcceptsMixed.new.call("foo", "bar", 3) { true } }
        assert_passes    { AcceptsMixed.new.call("foo", "bar", 3, 4, 5) { true } }
      end
      
      class Superclass
        include Handshake
        contract Superclass => boolean?
        def ==(other); self.class === other; end
      end
    
      class Subclass < Superclass; end
    
      class AcceptsSuperAndSub
        include Handshake
        contract Superclass => anything
        def call(cls); cls; end
      end
    
      should "accept subclasses in place of contracts defined to accept superclasses" do
        assert_violation { AcceptsSuperAndSub.new.call 3 }
        assert_passes    { AcceptsSuperAndSub.new.call Superclass.new }
        assert_passes    { AcceptsSuperAndSub.new.call Subclass.new }
        assert_passes    { Superclass.new == Subclass.new }
      end
      
      context "clause" do
        class AcceptsSimpleAssertion
          include Handshake
          equals_foo  = clause {|o| o == "foo"}
          contract [ equals_foo ] => anything
          def call(foo)
            return foo
          end
        end
    
        should "check contracts defined by custom clauses" do
          assert_violation { AcceptsSimpleAssertion.new.call }
          assert_violation { AcceptsSimpleAssertion.new.call 3 }
          assert_violation { AcceptsSimpleAssertion.new.call "bar", "bar" }
          assert_passes    { AcceptsSimpleAssertion.new.call "foo" }
        end
      end
      
      context "all?" do
        class AcceptsAll
          include Handshake
          equals_five = clause {|o| o == 5}
          contract all?(Integer, equals_five) => anything
          def initialize(n); end
        end
          
        should "check contracts that require all of the given clauses" do
          assert_violation { AcceptsAll.new "foo" }
          assert_violation { AcceptsAll.new 3 }
          assert_violation { AcceptsAll.new 5.0 }
          assert_passes    { AcceptsAll.new 5 }
        end
      end
      
      context "any?" do
        class AcceptsAny
          include Handshake
          equals_five = clause {|o| o == 5}
          equals_three = clause {|o| o == 3}
          contract any?(equals_five, equals_three) => anything
          def three_or_five(n); end
          contract any?(String, Integer, Symbol) => anything
          def str_int_sym(o); end
        end
          
        should "check contracts that require any of the given clauses" do
          assert_violation { AcceptsAny.new.three_or_five "foo" }
          assert_violation { AcceptsAny.new.three_or_five 7 }
          assert_violation { AcceptsAny.new.three_or_five 8, 9 }
          assert_passes    { AcceptsAny.new.three_or_five 3 }
          assert_passes    { AcceptsAny.new.three_or_five 5 }
          
          assert_violation { AcceptsAny.new.str_int_sym 5.3 }
          assert_raises(ArgumentError) { AcceptsAny.new.str_int_sym "str", 3, :sym }
          assert_passes    { AcceptsAny.new.str_int_sym "str" }
          assert_passes    { AcceptsAny.new.str_int_sym 3 }
          assert_passes    { AcceptsAny.new.str_int_sym :foo }
        end
      end
      
      context "not?" do
        class AcceptsNot
          include Handshake
          contract not?(String) => anything
          def initialize(not_str); end
        end
          
        should "check contracts that invert the given clause" do
          assert_violation { AcceptsNot.new "string" }
          assert_passes    { AcceptsNot.new 3 }
          assert_passes    { AcceptsNot.new :symbol }
        end
      end
      
      context "boolean?" do
        class AcceptsBoolean
          include Handshake
          contract boolean? => anything
          def initialize(bool); end
        end
          
        should "check contracts that ensure the given argument is a boolean" do
          assert_violation { AcceptsBoolean.new "foo" }
          assert_violation { AcceptsBoolean.new :foo }
          assert_passes    { AcceptsBoolean.new true }
          assert_passes    { AcceptsBoolean.new false }
        end
      end
      
      context "nonzero?" do
        class AcceptsNonzero
          include Handshake
          contract nonzero? => anything
          def initialize(nonzero); end
        end
          
        should "check contracts that ensure the given argument is nonzero" do
          assert_violation { AcceptsNonzero.new :foo }
          assert_violation { AcceptsNonzero.new 0 }
          assert_passes    { AcceptsNonzero.new 3 }
        end
      end
      
      context "hash_of?" do
        class AcceptsHashOf
          include Handshake
          contract hash_of?(Symbol, String) => anything
          def initialize(arg={}); end
        end
          
        def test_hash_of_sym_string
          assert_passes { AcceptsHashOf.new({}) }
          assert_passes { AcceptsHashOf.new({ :symbol => "String" }) }
          assert_violation { AcceptsHashOf.new({ :another => :symbol }) }
          assert_violation { AcceptsHashOf.new({ "two" => "strings" }) }
          assert_violation { AcceptsHashOf.new({ false => true }) }
        end
      end
      
      context "hash_with_keys" do
        class AcceptsHashWithKeys
          include Handshake
          contract hash_with_keys(:foo, :bar) => anything
          def initialize(options={}); end
        end
          
        def test_hash_with_keys_foo_bar
          assert_passes { AcceptsHashWithKeys.new({}) }
          assert_passes { AcceptsHashWithKeys.new({ :foo => "anything" }) }
          assert_passes { AcceptsHashWithKeys.new({ :bar => "anything" }) }
          assert_passes { AcceptsHashWithKeys.new({ :foo => "anything", :bar => "goes" }) }
          assert_violation { AcceptsHashWithKeys.new({ :arbitrary => "key" }) }
        end
      end
      
      context "hash_contract" do
        class AcceptsHashContract
          include Handshake
          contract hash_contract({ :foo => String, :bar => Integer, :baz => Symbol }) => anything
          def initialize(options={}); end
        end
          
        def test_hash_contract
          assert_passes    { AcceptsHashContract.new({}) }
          assert_passes    { AcceptsHashContract.new({ :foo => "bar"}) }
          assert_violation { AcceptsHashContract.new({ :foo => :bar}) }
          assert_passes    { AcceptsHashContract.new({ :bar => 3 }) }
          assert_violation { AcceptsHashContract.new({ :bar => "foo" }) }
          assert_passes    { AcceptsHashContract.new({ :baz => :foo }) }
          assert_violation { AcceptsHashContract.new({ :baz => "baz" }) }
          assert_passes    { AcceptsHashContract.new({ :foo => "bar", :bar => 3, :baz => :qux }) }
        end
      end
      
      context "responds_to?" do
        class AcceptsRespondsTo
          include Handshake
          contract responds_to?(:each, :first) => anything
          def initialize(duck_array); end
        end
          
        def test_responds_to_each_first
          assert_violation { AcceptsRespondsTo.new(Object.new) }
          assert_violation { AcceptsRespondsTo.new "foo" }
          assert_violation { AcceptsRespondsTo.new 3 }
          assert_passes    { AcceptsRespondsTo.new([]) }
        end
      end
      
      context "is?" do
        class AcceptsIsA
          include Handshake
          contract is?(:String) => is?(:Symbol)
          def call_is_a(o); return o.to_s.to_sym; end
        end
          
        def test_accepts_is_string_symbol
          assert_violation { AcceptsIsA.new.call_is_a(3) }
          assert_violation { AcceptsIsA.new.call_is_a(:foo) }
          assert_passes    { AcceptsIsA.new.call_is_a("foo") }
        end
      end
    end
    
    context "before" do
      class SimpleBeforeCondition
        include Handshake
        before { assert false }
        def call_fails; end
        def call_passes; end
      end
      class ExtendsSimpleBeforeCondition < SimpleBeforeCondition; end
    
      def test_simple_before_condition
        assert_equal(1, SimpleBeforeCondition.method_contracts.length)
        assert_not_nil(SimpleBeforeCondition.method_contracts[:call_fails])
        assert_violation { SimpleBeforeCondition.new.call_fails }
        assert_passes    { SimpleBeforeCondition.new.call_passes }
        assert_equal(2, ExtendsSimpleBeforeCondition.method_contracts.length)
        assert_not_nil(ExtendsSimpleBeforeCondition.method_contracts[:call_fails])
        assert_violation { ExtendsSimpleBeforeCondition.new.call_fails }
        assert_passes    { ExtendsSimpleBeforeCondition.new.call_passes }
      end
      
      class ScopedBeforeCondition
        include Handshake
        def initialize(bool); @bool = bool; end
        before { assert @bool }
        def call; end
      end
    
      def test_scoped_before_condition
        assert_violation { ScopedBeforeCondition.new(false).call }
        assert_passes    { ScopedBeforeCondition.new(true).call }
      end
      
      class BeforeClauseAssert
        include Handshake
    
        before do |arg|
          assert_equal("foo", arg, "arg must equal foo")
        end
        
        def call(arg)
          arg
        end
      end
    
      def test_before_clause_assert
        assert_violation { BeforeClauseAssert.new.call 3 }
        assert_violation { BeforeClauseAssert.new.call "bar" }
        assert_passes    { BeforeClauseAssert.new.call "foo" }
      end
    end
    
    context "after" do
      class SimpleAfterCondition
        include Handshake
        after { |returned| assert returned }
        def call(bool); bool; end
      end
    
      def test_simple_after_condition
        assert_equal(1, SimpleAfterCondition.method_contracts.length)
        assert_not_nil(SimpleAfterCondition.method_contracts[:call])
        assert_violation { SimpleAfterCondition.new.call(false) }
        assert_violation { SimpleAfterCondition.new.call(nil)   }
        assert_passes    { SimpleAfterCondition.new.call(true)  }
        assert_passes    { SimpleAfterCondition.new.call("foo") }
      end
    end
    
    context "around" do
      class SimpleAroundCondition
        include Handshake
        around {|arg| assert(!arg) }
        def call(bool); bool; end
      end
    
      def test_simple_around_condition
        [ 1, :foo, true, "bar", 8.3 ].each do |val|
          assert_violation { SimpleAroundCondition.new.call(val) }
        end

        [ false, nil ].each do |val|
          assert_passes { SimpleAroundCondition.new.call(val) }
        end
      end
    end
    
    context "contract_reader, contract_writer, contract_accessor" do
      class ContractAccessor
        include Handshake
        contract_reader :foo => String
        contract_writer :bar => Integer
        contract_accessor :baz => Symbol, :qux => Float
        def initialize(foo=nil); @foo = foo; end
      end
    
      def test_contract_accessor
        assert_equal(6, ContractAccessor.method_contracts.length)
        assert_violation { ContractAccessor.new.foo }
        assert_violation { ContractAccessor.new(3).foo }
        assert_passes    { ContractAccessor.new("foo").foo }
        assert_violation { ContractAccessor.new.bar = "bar" }
        assert_passes    { ContractAccessor.new.bar = 3 }
        assert_violation { ContractAccessor.new.baz = "3" }
        assert_violation { ContractAccessor.new.qux = 3 }
        assert_passes    { ContractAccessor.new.baz = :baz }
        assert_passes    { ContractAccessor.new.qux = 3.3 }
      end
    end
    
    context "checked_self" do
      class CheckedSelf
        include Handshake
        def call_checked(obj)
          checked_self.call(obj)
        end
        def call_unchecked(obj)
          call(obj)
        end
        contract String => anything
        def call(str); str; end
      end
    
      class ExtendsCheckedSelf < CheckedSelf
        private
        contract Numeric => anything
        def call(n); n; end
      end
    
      def test_checked_self
        assert_violation { CheckedSelf.new.call(5) }
        assert_violation { CheckedSelf.new.call_checked(5) }
        assert_passes    { CheckedSelf.new.call_unchecked(5) }
        assert_passes    { CheckedSelf.new.call_checked("foo") }
        assert_violation { ExtendsCheckedSelf.new.call_checked("foo") }
        assert_passes    { ExtendsCheckedSelf.new.call_checked(5) }
        assert_passes    { ExtendsCheckedSelf.new.call_unchecked("foo") }
      end
    end
    
    context "Block" do
      class CheckedBlockContract
        include Handshake
    
        contract [ anything, Block(String => Integer) ] => Integer
        def yields(value); yield(value); end
    
        contract [ anything, Block(String => Integer) ] => Integer
        def calls(value, &block); block.call(value); end
      end
    
      def test_checked_block_contract_yields
        assert_violation { CheckedBlockContract.new.yields("3") {|s| s.to_s } }
        assert_violation { CheckedBlockContract.new.yields("3") {|s| "foo" } }
        assert_violation { CheckedBlockContract.new.yields(3) {|s| s.to_i} }
        assert_passes    { CheckedBlockContract.new.yields("3") {|s| 3 } }
        assert_passes    { CheckedBlockContract.new.yields("3") {|s| s.to_i } }
      end
    
      def test_checked_block_contract_calls
        assert_violation { CheckedBlockContract.new.calls("3") {|s| s.to_s } }
        assert_violation { CheckedBlockContract.new.calls("3") {|s| "foo" } }
        assert_violation { CheckedBlockContract.new.calls(3) {|s| s.to_i} }
        assert_passes    { CheckedBlockContract.new.calls("3") {|s| 3 } }
        assert_passes    { CheckedBlockContract.new.calls("3") {|s| s.to_i } }
      end
    end
    
    context "suppress!" do
      teardown { Handshake.enable! }

      class ComprehensiveContracts
        include Handshake

        invariant("foo must always be true") { @foo == true }

        contract /foo/ => /bar/ 
        before do |arg|
          assert_equal "foo", arg
        end
        after do |arg, returned|
          assert_equal "bar", returned
        end
        def call(str); "baz"; end
      end

      should "be suppressed after declaration" do
        Handshake.suppress!
        assert Handshake.suppressed?
      end

      should "not enforce contracts" do
        Handshake.suppress!
        assert_nothing_raised { ComprehensiveContracts.new }
        assert_nothing_raised { ComprehensiveContracts.new.call 3 }
      end
    end
  end
end