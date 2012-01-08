require 'rspec'
require 'handshake'
require 'handshake/matchers'

describe Handshake do
  describe "invariant" do
    class InvariantDeclarations
      include Handshake
      invariant { true }
    end
  
    class ExtendsInvariantDeclarations < InvariantDeclarations
      invariant { true }
    end
  
    it "should correctly track the list of invariants in superclasses and subclasses" do
      InvariantDeclarations.invariants.length.should == 1
      ExtendsInvariantDeclarations.invariants.length.should == 2
    end
  
    class NonFunctionalArray < Array
      include Handshake
      invariant { false }
    end
  
    it "should fail a very simple invariant check" do
      lambda { NonFunctionalArray.new }.should violate_contract
    end
    
    class PositiveBalance
      include Handshake
      invariant { @balance > 0 }
      attr_accessor :balance
      def initialize(balance); @balance = balance; end
    end
  
    it "should check invariants that leverage new instance methods" do
      lambda { PositiveBalance.new(-10) }.should violate_contract
      lambda { PositiveBalance.new 0 }.should violate_contract
      lambda { PositiveBalance.new 10 }.should_not violate_contract
      lambda { PositiveBalance.new(10); pb.balance = -10 }.should_not violate_contract
    end
  
    class NonEmptyArray < Array
      include Handshake
      invariant { not empty? }
    end
    class ExtendsNonEmptyArray < NonEmptyArray; end
    
    it "should check invariants that leverage inherited instance methods" do
      lambda { NonEmptyArray.new }.should violate_contract
      lambda { NonEmptyArray.new [1] }.should_not violate_contract
      lambda { ExtendsNonEmptyArray.new }.should violate_contract
      lambda { ExtendsNonEmptyArray.new [1] }.should_not violate_contract
      lambda { NonEmptyArray.new([1]).pop }.should violate_contract
    end
  end
  
  describe "contract" do
    class MethodDeclarations
      include Handshake
      contract :accepts_str, String => anything
      contract :accepts_int, Integer => anything
    end
    class ExtendsMethodDeclarations < MethodDeclarations; end
  
    it "should tie contract declaration to the method name to which they apply, and apply them to subclasses" do
      MethodDeclarations.method_contracts.should have_key(:accepts_str)
      MethodDeclarations.method_contracts.should have_key(:accepts_int)
      ExtendsMethodDeclarations.method_contracts.should have_key(:accepts_str)
      ExtendsMethodDeclarations.method_contracts.should have_key(:accepts_int)
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
  
    it "should check simple type contracts for a single method argument" do
      lambda { AcceptsString.new 3 }.should violate_contract
      lambda { AcceptsString.new :foo }.should violate_contract
      lambda { AcceptsString.new "string" }.should_not violate_contract
      lambda { AcceptsString.new("foo").str = 3 }.should violate_contract
    end

    it "should check contracts descended from a superclass with contracts" do
      lambda { ExtendsAcceptsString.new 3 }.should violate_contract
      lambda { ExtendsAcceptsString.new :foo }.should violate_contract
      lambda { ExtendsAcceptsString.new "string" }.should_not violate_contract
      lambda { ExtendsAcceptsString.new("foo").str = 3 }.should violate_contract
    end

    it "should check contracts that override a superclass's defined contract" do
      lambda { AcceptsIntegerInstead.new("foo") }.should violate_contract
      lambda { AcceptsIntegerInstead.new 3 }.should_not violate_contract
      lambda { AcceptsSymbolInstead.new "foo" }.should violate_contract
      lambda { AcceptsSymbolInstead.new 3 }.should violate_contract
      lambda { AcceptsSymbolInstead.new :foo }
    end
    
    class ReturnsString
      include Handshake
      contract anything => String
      def call(val); val; end
    end
    class ExtendsReturnsString < ReturnsString; end
  
    it "should check simple type contracts for a single method return value" do
      lambda { ReturnsString.new.call(1) }.should violate_contract
      lambda { ReturnsString.new.call(true) }.should violate_contract
      lambda { ReturnsString.new.call("foo") }.should_not violate_contract
    end

    it "should check simple type contracts for descendants of a superclass with contracts" do
      lambda { ExtendsReturnsString.new.call(1) }.should violate_contract
      lambda { ExtendsReturnsString.new.call(true) }.should violate_contract
      lambda { ExtendsReturnsString.new.call("foo") }.should_not violate_contract
    end
    
    class ReturnsMultiple
      include Handshake
      contract [ String, Integer ] => anything
      def call(arg1, arg2); return arg1, arg2; end
    end
  
    it "should check simple type contracts for multiple method arguments" do
      lambda { ReturnsMultiple.new.call("foo", "foo") }.should violate_contract
      lambda { ReturnsMultiple.new.call(3, 3) }.should violate_contract
      lambda { ReturnsMultiple.new.call("foo", 3) }.should_not violate_contract
    end
    
    class AcceptsVarargs
      include Handshake
      contract [[ String ]] => anything
      def initialize(*strs); @strs = strs; end
    end
  
    it "should check simple type contracts for methods that accept varargs" do
      lambda { AcceptsVarargs.new }.should_not violate_contract
      lambda { AcceptsVarargs.new(1, 2, 3) }.should violate_contract
      lambda { AcceptsVarargs.new("foo", 1, 2) }.should violate_contract
      lambda { AcceptsVarargs.new(:foo, "foo") }.should violate_contract
      lambda { AcceptsVarargs.new("foo") }.should_not violate_contract
      lambda { AcceptsVarargs.new("foo1", "foo2") }.should_not violate_contract
    end
    
    class AcceptsBlock
      include Handshake
      contract Block => anything
      def call1; end
      contract Block => anything
      def call2(&block); end
    end
  
    it "should check simple type contracts that ensure methods can accept a block" do
      lambda { AcceptsBlock.new.call1 }.should violate_contract
      lambda { AcceptsBlock.new.call2 }.should violate_contract
      lambda { AcceptsBlock.new.call1 { true } }.should_not violate_contract
      lambda { AcceptsBlock.new.call2 { true } }.should_not violate_contract
      lambda { AcceptsBlock.new.call1 { "foo" } }.should_not violate_contract
      lambda { AcceptsBlock.new.call1("foo") }.should violate_contract
      lambda { AcceptsBlock.new.call2("foo") }.should violate_contract
    end
    
    class AcceptsWriter
      include Handshake
      contract String => anything
      def val=(str); @str = str; end
    end
  
    it "should check simple type contracts for writer methods" do
      lambda { AcceptsWriter.new.val = 3 }.should violate_contract
      lambda { AcceptsWriter.new.val = :foo }.should violate_contract
      lambda { AcceptsWriter.new.val = "foo" }.should_not violate_contract
    end
    
    class AcceptsMixed
      include Handshake
      contract [ String, String, [ Integer ], Block ] => String
      def call(str1, str2, *ints, &block); "foo"; end
    end
  
    it "should check simple type contracts for methods that accept multiple arguments, varargs, and a block" do
      lambda { AcceptsMixed.new.call }.should violate_contract
      lambda { AcceptsMixed.new.call 3 }.should violate_contract
      lambda { AcceptsMixed.new.call "foo" }.should violate_contract
      lambda { AcceptsMixed.new.call "foo", 3 }.should violate_contract
      lambda { AcceptsMixed.new.call "foo", "bar" }.should violate_contract
      lambda { AcceptsMixed.new.call("foo", "bar") { true } }.should_not violate_contract
      lambda { AcceptsMixed.new.call("foo", "bar", 3) { true } }.should_not violate_contract
      lambda { AcceptsMixed.new.call("foo", "bar", 3, 4, 5) { true } }.should_not violate_contract
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
  
    it "should accept subclasses in place of contracts defined to accept superclasses" do
      lambda { AcceptsSuperAndSub.new.call 3 }.should violate_contract
      lambda { AcceptsSuperAndSub.new.call Superclass.new }.should_not violate_contract
      lambda { AcceptsSuperAndSub.new.call Subclass.new }.should_not violate_contract
      lambda { Superclass.new == Subclass.new }.should_not violate_contract
    end
    
    describe "clause" do
      class AcceptsSimpleAssertion
        include Handshake
        equals_foo  = clause {|o| o == "foo"}
        contract [ equals_foo ] => anything
        def call(foo)
          return foo
        end
      end
  
      it "should check contracts defined by custom clauses" do
        lambda { AcceptsSimpleAssertion.new.call }.should violate_contract
        lambda { AcceptsSimpleAssertion.new.call 3 }.should violate_contract
        lambda { AcceptsSimpleAssertion.new.call "bar", "bar" }.should violate_contract
        lambda { AcceptsSimpleAssertion.new.call "foo" }.should_not violate_contract
      end
    end
    
    describe "all?" do
      class AcceptsAll
        include Handshake
        equals_five = clause {|o| o == 5}
        contract all?(Integer, equals_five) => anything
        def initialize(n); end
      end
        
      it "should check contracts that require all of the given clauses" do
        lambda { AcceptsAll.new "foo" }.should violate_contract
        lambda { AcceptsAll.new 3 }.should violate_contract
        lambda { AcceptsAll.new 5.0 }.should violate_contract
        lambda { AcceptsAll.new 5 }.should_not violate_contract
      end
    end
    
    describe "any?" do
      class AcceptsAny
        include Handshake
        equals_five = clause {|o| o == 5}
        equals_three = clause {|o| o == 3}
        contract any?(equals_five, equals_three) => anything
        def three_or_five(n); end
        contract any?(String, Integer, Symbol) => anything
        def str_int_sym(o); end
      end
        
      it "should check contracts that require any of the given clauses" do
        lambda { AcceptsAny.new.three_or_five "foo" }.should violate_contract
        lambda { AcceptsAny.new.three_or_five 7 }.should violate_contract
        lambda { AcceptsAny.new.three_or_five 8, 9 }.should violate_contract
        lambda { AcceptsAny.new.three_or_five 3 }.should_not violate_contract
        lambda { AcceptsAny.new.three_or_five 5 }.should_not violate_contract
        
        lambda { AcceptsAny.new.str_int_sym 5.3 }.should violate_contract
        lambda { AcceptsAny.new.str_int_sym "str", 3, :sym }.should raise_error(ArgumentError)
        lambda { AcceptsAny.new.str_int_sym "str" }.should_not violate_contract
        lambda { AcceptsAny.new.str_int_sym 3 }.should_not violate_contract
        lambda { AcceptsAny.new.str_int_sym :foo }.should_not violate_contract
      end
    end
    
    describe "not?" do
      class AcceptsNot
        include Handshake
        contract not?(String) => anything
        def initialize(not_str); end
      end
        
      it "should check contracts that invert the given clause" do
        lambda { AcceptsNot.new "string" }.should violate_contract
        lambda { AcceptsNot.new 3 }.should_not violate_contract
        lambda { AcceptsNot.new :symbol }.should_not violate_contract
      end
    end
    
    describe "boolean?" do
      class AcceptsBoolean
        include Handshake
        contract boolean? => anything
        def initialize(bool); end
      end
        
      it "should check contracts that ensure the given argument is a boolean" do
        lambda { AcceptsBoolean.new "foo" }.should violate_contract
        lambda { AcceptsBoolean.new :foo }.should violate_contract
        lambda { AcceptsBoolean.new true }.should_not violate_contract
        lambda { AcceptsBoolean.new false }.should_not violate_contract
      end
    end
    
    describe "nonzero?" do
      class AcceptsNonzero
        include Handshake
        contract nonzero? => anything
        def initialize(nonzero); end
      end
        
      it "should check contracts that ensure the given argument is nonzero" do
        lambda { AcceptsNonzero.new :foo }.should violate_contract
        lambda { AcceptsNonzero.new 0 }.should violate_contract
        lambda { AcceptsNonzero.new 3 }.should_not violate_contract
      end
    end
    
    describe "hash_of?" do
      class AcceptsHashOf
        include Handshake
        contract hash_of?(Symbol, String) => anything
        def initialize(arg={}); end
      end
        
      it "should check contracts that impose a structure on a given hash" do
        lambda { AcceptsHashOf.new({}) }.should_not violate_contract
        lambda { AcceptsHashOf.new( :symbol => "String" ) }.should_not violate_contract
        lambda { AcceptsHashOf.new( :another => :symbol ) }.should violate_contract
        lambda { AcceptsHashOf.new( "two" => "strings" ) }.should violate_contract
        lambda { AcceptsHashOf.new( false => true ) }.should violate_contract
      end
    end
    
    describe "hash_with_keys" do
      class AcceptsHashWithKeys
        include Handshake
        contract hash_with_keys(:foo, :bar) => anything
        def initialize(options={}); end
      end
        
      it "should check contracts that require hashes contain certain keys" do
        lambda { AcceptsHashWithKeys.new({}) }.should_not violate_contract
        lambda { AcceptsHashWithKeys.new( :foo => "anything" ) }.should_not violate_contract
        lambda { AcceptsHashWithKeys.new( :bar => "anything" ) }.should_not violate_contract
        lambda { AcceptsHashWithKeys.new( :foo => "anything", :bar => "goes" ) }.should_not violate_contract
        lambda { AcceptsHashWithKeys.new( :arbitrary => "key" ) }.should violate_contract
      end
    end
    
    describe "hash_contract" do
      class AcceptsHashContract
        include Handshake
        contract hash_contract({ :foo => String, :bar => Integer, :baz => Symbol }) => anything
        def initialize(options={}); end
      end
        
      it "should check contracts for fairly complex hash signatures" do
        lambda { AcceptsHashContract.new({}) }.should_not violate_contract
        lambda { AcceptsHashContract.new( :foo => "bar" ) }.should_not violate_contract
        lambda { AcceptsHashContract.new( :foo => :bar ) }.should violate_contract
        lambda { AcceptsHashContract.new( :bar => 3 ) }.should_not violate_contract
        lambda { AcceptsHashContract.new( :bar => "foo" ) }.should violate_contract
        lambda { AcceptsHashContract.new( :baz => :foo ) }.should_not violate_contract
        lambda { AcceptsHashContract.new( :baz => "baz" ) }.should violate_contract
        lambda { AcceptsHashContract.new( :foo => "bar", :bar => 3, :baz => :qux ) }.should_not violate_contract
      end
    end
    
    describe "responds_to?" do
      class AcceptsRespondsTo
        include Handshake
        contract responds_to?(:each, :first) => anything
        def initialize(duck_array); end
      end
        
      it "should check duck-type contracts for arguments that should respond to certain methods" do
        lambda { AcceptsRespondsTo.new(Object.new) }.should violate_contract
        lambda { AcceptsRespondsTo.new "foo" }.should violate_contract
        lambda { AcceptsRespondsTo.new 3 }.should violate_contract
        lambda { AcceptsRespondsTo.new([]) }.should_not violate_contract
      end
    end
    
    describe "is?" do
      class AcceptsIsA
        include Handshake
        contract is?(:String) => is?(:Symbol)
        def call_is_a(o); return o.to_s.to_sym; end
      end

      it "should check contracts that allow you to defer the loading of a constant until check time" do
        lambda { AcceptsIsA.new.call_is_a(3) }.should violate_contract
        lambda { AcceptsIsA.new.call_is_a(:foo) }.should violate_contract
        lambda { AcceptsIsA.new.call_is_a("foo") }.should_not violate_contract
      end
    end
  end
  
  describe "before" do
    class SimpleBeforeCondition
      include Handshake
      before { assert false }
      def call_fails; end
      def call_passes; end
    end
    class ExtendsSimpleBeforeCondition < SimpleBeforeCondition; end
  
    it "should check simple before conditions" do
      SimpleBeforeCondition.method_contracts.length.should == 1
      SimpleBeforeCondition.method_contracts[:call_fails].should_not be_nil
      lambda { SimpleBeforeCondition.new.call_fails }.should violate_contract
    end

    it "should generate and cache no-op contracts" do
      lambda { SimpleBeforeCondition.new.call_passes }.should_not violate_contract
      SimpleBeforeCondition.method_contracts.length.should == 2
      SimpleBeforeCondition.method_contracts[:call_passes].should_not be_nil
    end

    it "should check before-conditions inherited from superclasses" do
      lambda { ExtendsSimpleBeforeCondition.new.call_fails }.should violate_contract
      lambda { ExtendsSimpleBeforeCondition.new.call_passes }.should_not violate_contract
    end
    
    class ScopedBeforeCondition
      include Handshake
      def initialize(bool); @bool = bool; end
      before { assert @bool }
      def call; end
    end
  
    it "should check before-conditions that require the use of instance variables" do
      lambda { ScopedBeforeCondition.new(false).call }.should violate_contract
      lambda { ScopedBeforeCondition.new(true).call }.should_not violate_contract
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
  
    it "should check before-conditions that require the use of arguments" do
      lambda { BeforeClauseAssert.new.call 3 }.should violate_contract
      lambda { BeforeClauseAssert.new.call "bar" }.should violate_contract
      lambda { BeforeClauseAssert.new.call "foo" }.should_not violate_contract
    end
  end
  
  describe "after" do
    class SimpleAfterCondition
      include Handshake
      after { |returned| assert returned }
      def call(bool); bool; end
    end
  
    it "should check simple after-conditions" do
      SimpleAfterCondition.method_contracts.length.should == 1
      SimpleAfterCondition.method_contracts[:call].should_not be_nil
      lambda { SimpleAfterCondition.new.call(false) }.should violate_contract
      lambda { SimpleAfterCondition.new.call(nil)   }.should violate_contract
      lambda { SimpleAfterCondition.new.call(true)  }.should_not violate_contract
      lambda { SimpleAfterCondition.new.call("foo") }.should_not violate_contract
    end
  end
  
  describe "around" do
    class SimpleAroundCondition
      include Handshake
      around {|arg| assert(!arg) }
      def call(bool); bool; end
    end
  
    it "should check simple around-conditions" do
      [ 1, :foo, true, "bar", 8.3 ].each do |val|
        lambda { SimpleAroundCondition.new.call(val) }.should violate_contract
      end

      [ false, nil ].each do |val|
        lambda { SimpleAroundCondition.new.call(val) }.should_not violate_contract
      end
    end
  end
  
  describe "contract_reader, contract_writer, contract_accessor" do
    class ContractAccessor
      include Handshake
      contract_reader :foo => String
      contract_writer :bar => Integer
      contract_accessor :baz => Symbol, :qux => Float
      def initialize(foo=nil); @foo = foo; end
    end
  
    it "should supply contract-enforced readers, writers, and accessors" do
      ContractAccessor.method_contracts.length.should == 6

      lambda { ContractAccessor.new.foo }.should violate_contract
      lambda { ContractAccessor.new(3).foo }.should violate_contract
      lambda { ContractAccessor.new("foo").foo }.should_not violate_contract
      lambda { ContractAccessor.new.bar = "bar" }.should violate_contract
      lambda { ContractAccessor.new.bar = 3 }.should_not violate_contract
      lambda { ContractAccessor.new.baz = "3" }.should violate_contract
      lambda { ContractAccessor.new.qux = 3 }.should violate_contract
      lambda { ContractAccessor.new.baz = :baz }.should_not violate_contract
      lambda { ContractAccessor.new.qux = 3.3 }.should_not violate_contract
    end
  end
  
  describe "checked_self" do
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
  
    it "should expose a checked_self method for checking contracts of internal method calls" do
      lambda { CheckedSelf.new.call(5) }.should violate_contract
      lambda { CheckedSelf.new.call_checked(5) }.should violate_contract
      lambda { CheckedSelf.new.call_unchecked(5) }.should_not violate_contract
      lambda { CheckedSelf.new.call_checked("foo") }.should_not violate_contract
    end

    it "should extend the checked_self method to subclasses" do
      lambda { ExtendsCheckedSelf.new.call_checked("foo") }.should violate_contract
      lambda { ExtendsCheckedSelf.new.call_checked(5) }.should_not violate_contract
      lambda { ExtendsCheckedSelf.new.call_unchecked("foo") }.should_not violate_contract
    end
  end
  
  describe "Block" do
    class CheckedBlockContract
      include Handshake
  
      contract [ anything, Block(String => Integer) ] => Integer
      def yields(value); yield(value); end
  
      contract [ anything, Block(String => Integer) ] => Integer
      def calls(value, &block); block.call(value); end
    end
  
    it "should check conditions of blocks given to methods that yield" do
      lambda { CheckedBlockContract.new.yields("3") {|s| s.to_s } }.should violate_contract
      lambda { CheckedBlockContract.new.yields("3") {|s| "foo" } }.should violate_contract
      lambda { CheckedBlockContract.new.yields(3) {|s| s.to_i} }.should violate_contract
      lambda { CheckedBlockContract.new.yields("3") {|s| 3 } }.should_not violate_contract
      lambda { CheckedBlockContract.new.yields("3") {|s| s.to_i } }.should_not violate_contract
    end
  
    it "should check conditions of blocks given to methods that call the given block explicitly" do
      lambda { CheckedBlockContract.new.calls("3") {|s| s.to_s } }.should violate_contract
      lambda { CheckedBlockContract.new.calls("3") {|s| "foo" } }.should violate_contract
      lambda { CheckedBlockContract.new.calls(3) {|s| s.to_i} }.should violate_contract
      lambda { CheckedBlockContract.new.calls("3") {|s| 3 } }.should_not violate_contract
      lambda { CheckedBlockContract.new.calls("3") {|s| s.to_i } }.should_not violate_contract
    end
  end
  
  describe "suppress!" do
    after { Handshake.enable! }

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

    it "should be suppressed after declaration" do
      Handshake.suppress!
      Handshake.should be_suppressed
    end

    it "should not enforce contracts" do
      Handshake.suppress!
      lambda { ComprehensiveContracts.new }.should_not raise_error
      lambda { ComprehensiveContracts.new.call 3 }.should_not raise_error
    end
  end
end