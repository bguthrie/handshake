require 'test/unit'
require 'contract'

module Test
  module Unit
    module Assertions
      def assert_violation(&block)
        assert_raise(ContractViolation, &block)
      end
    
      def assert_passes(&block)
        assert_nothing_raised(&block)
      end
    end
  end
end

class TestContract < Test::Unit::TestCase

  class InvariantDeclarations
    include Contract
    invariant { true }
  end

  class ExtendsInvariantDeclarations < InvariantDeclarations
    invariant { true }
  end

  def test_invariant_declarations
    assert_equal 1, InvariantDeclarations.invariants.length
    assert_equal 2, ExtendsInvariantDeclarations.invariants.length
  end

  class NonFunctionalArray < Array
    include Contract
    invariant { false }
  end

  def test_basic_invariant
    assert_violation { NonFunctionalArray.new }
  end

  class NonEmptyArray < Array
    include Contract
    invariant { not empty? }
  end
  class ExtendsNonEmptyArray < NonEmptyArray; end
  
  class PositiveBalance
    include Contract
    invariant { @balance > 0 }
    attr_accessor :balance
    def initialize(balance); @balance = balance; end
  end

  def test_invariants
    assert_violation { NonEmptyArray.new }
    assert_passes    { NonEmptyArray.new [1] }
    assert_violation { ExtendsNonEmptyArray.new }
    assert_passes    { ExtendsNonEmptyArray.new [1] }

    assert_violation { NonEmptyArray.new([1]).pop }

    assert_violation { PositiveBalance.new -10 }
    assert_violation { PositiveBalance.new 0 }
    assert_passes    { PositiveBalance.new 10 }
    assert_violation { 
      pb = PositiveBalance.new(10); pb.balance = -10
    }
  end

  class MethodDeclarations
    include Contract
    method :accepts => [ String ]
    method :accepts_int, :accepts => [ Integer ]
    def accepts_str(str); str; end
    def accepts_int(int); int; end
  end
  class ExtendsMethodDeclarations < MethodDeclarations; end

  def test_method_declarations
    assert MethodDeclarations.checked_methods.has_key?(:accepts_str)
    assert MethodDeclarations.checked_methods.has_key?(:accepts_int)
    assert ExtendsMethodDeclarations.checked_methods.has_key?(:accepts_str)
    assert ExtendsMethodDeclarations.checked_methods.has_key?(:accepts_int)
  end

  class AcceptsString
    include Contract
    method :initialize, :accepts => [ String ]
    def initialize(str); @str = str; end
    method :accepts => [ String ]
    def str=(str); @str = str; end
  end
  class ExtendsAcceptsString < AcceptsString; end

  def test_method_accepts
    assert_violation { AcceptsString.new 3 }
    assert_violation { AcceptsString.new :foo }
    assert_passes    { AcceptsString.new "string" }
    assert_violation { AcceptsString.new("foo").str = 3 }
    assert_violation { ExtendsAcceptsString.new 3 }
    assert_violation { ExtendsAcceptsString.new :foo }
    assert_passes    { ExtendsAcceptsString.new "string" }
    assert_violation { ExtendsAcceptsString.new("foo").str = 3 }
  end

  class ReturnsString
    include Contract
    method :returns => [ String ]
    def call(val); val; end
  end
  class ExtendsReturnsString < ReturnsString; end

  def test_method_returns
    assert_violation { ReturnsString.new.call(1) }
    assert_violation { ReturnsString.new.call(true) }
    assert_passes    { ReturnsString.new.call("foo") }
    assert_violation { ExtendsReturnsString.new.call(1) }
    assert_violation { ExtendsReturnsString.new.call(true) }
    assert_passes    { ExtendsReturnsString.new.call("foo") }
  end

  class ReturnsMultiple
    include Contract
    method :returns => [ String, Integer ]
    def call(arg1, arg2); return arg1, arg2; end
  end

  def test_method_returns_multiple
    assert_violation { ReturnsMultiple.new.call("foo", "foo") }
    assert_violation { ReturnsMultiple.new.call(3, 3) }
    assert_passes    { ReturnsMultiple.new.call("foo", 3) }
  end

  class AcceptsVarargs
    include Contract
    method :accepts => [[ String ]]
    def initialize(*strs); @strs = strs; end
  end

  def test_method_accepts_varargs
    assert_passes    { AcceptsVarargs.new }
    assert_violation { AcceptsVarargs.new(1, 2, 3) }
    assert_violation { AcceptsVarargs.new("foo", 1, 2) }
    assert_violation { AcceptsVarargs.new(:foo, "foo") }
    assert_passes    { AcceptsVarargs.new("foo") }
    assert_passes    { AcceptsVarargs.new("foo1", "foo2") }
  end

  class AcceptsBlock
    include Contract
    method :accepts => [ Block ]
    def call1; end
    method :accepts => [ Block ]
    def call2(&block); end
  end

  def test_method_accepts_block
    assert_violation { AcceptsBlock.new.call1 }
    assert_violation { AcceptsBlock.new.call2 }
    assert_passes    { AcceptsBlock.new.call1 { true } }
    assert_passes    { AcceptsBlock.new.call2 { true } }
    assert_passes    { AcceptsBlock.new.call1 { "foo" } }
    assert_violation { AcceptsBlock.new.call1("foo") }
    assert_violation { AcceptsBlock.new.call2("foo") }
  end

  # EXCELSIOR!
  class AcceptsMixed
    include Contract
    method :accepts => [ String, String, [ Integer ], Block ],
           :returns => [ String ]
    def call(str1, str2, *ints, &block); "foo"; end
  end

  def test_method_mixed
    assert_violation { AcceptsMixed.new.call }
    assert_violation { AcceptsMixed.new.call 3 }
    assert_violation { AcceptsMixed.new.call "foo" }
    assert_violation { AcceptsMixed.new.call "foo", 3 }
    assert_violation { AcceptsMixed.new.call "foo", "bar" }
    assert_passes    { AcceptsMixed.new.call("foo", "bar") { true } }
    assert_passes    { AcceptsMixed.new.call("foo", "bar", 3) { true } }
    assert_passes    { AcceptsMixed.new.call("foo", "bar", 3, 4, 5) { true } }
  end

  class AcceptsSimpleAssertion
    include Contract
    equals_foo  = assert {|o| o == "foo"}
    equals_foo2 = Clause.new {|o| o == "foo"}
    method :accepts => [ equals_foo, equals_foo2 ]
    def call(foo, foo2)
      return foo, foo2
    end
  end

  def test_method_simple_assertion
    assert_violation { AcceptsSimpleAssertion.new.call }
    assert_violation { AcceptsSimpleAssertion.new.call "bar", "bar" }
    assert_violation { AcceptsSimpleAssertion.new.call "foo" }
    assert_passes    { AcceptsSimpleAssertion.new.call "foo", "foo" }
  end

  class AcceptsAll
    include Contract
    equals_five = assert {|o| o == 5}
    method :accepts => [ all?(Integer, equals_five) ]
    def initialize(n); end
  end

  def test_accepts_all_of
    assert_violation { AcceptsAll.new "foo" }
    assert_violation { AcceptsAll.new 3 }
    assert_violation { AcceptsAll.new 5.0 }
    assert_passes    { AcceptsAll.new 5 }
  end

  class AcceptsAny
    include Contract
    equals_five = assert {|o| o == 5}
    equals_three = assert {|o| o == 3}
    method :accepts => [ any?(equals_five, equals_three) ]
    def three_or_five(n); end
    method :accepts => [ any?(String, Integer, Symbol) ]
    def str_int_sym(o); end
  end

  def test_accepts_any_of
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

  class AcceptsNot
    include Contract
    method :accepts => [ not?(String) ]
    def initialize(not_str); end
  end

  def test_accepts_not_string
    assert_violation { AcceptsNot.new "string" }
    assert_passes    { AcceptsNot.new 3 }
    assert_passes    { AcceptsNot.new :symbol }
  end

  class AcceptsBoolean
    include Contract
    method :accepts => [ boolean? ]
    def initialize(bool); end
  end

  def test_accepts_boolean
    assert_violation { AcceptsBoolean.new "foo" }
    assert_violation { AcceptsBoolean.new :foo }
    assert_passes    { AcceptsBoolean.new true }
    assert_passes    { AcceptsBoolean.new false }
  end

  class AcceptsNonzero
    include Contract
    method :accepts => [ nonzero? ]
    def initialize(nonzero); end
  end

  def test_accepts_nonzero
    assert_violation { AcceptsNonzero.new :foo }
    assert_violation { AcceptsNonzero.new 0 }
    assert_passes    { AcceptsNonzero.new 3 }
  end

  class AcceptsHashOf
    include Contract
    method :accepts => [ hash_of?(Symbol, String) ]
    def initialize(arg={}); end
  end

  def test_hash_of_sym_string
    assert_passes { AcceptsHashOf.new({}) }
    assert_passes { AcceptsHashOf.new({ :symbol => "String" }) }
    assert_violation { AcceptsHashOf.new({ :another => :symbol }) }
    assert_violation { AcceptsHashOf.new({ "two" => "strings" }) }
    assert_violation { AcceptsHashOf.new({ false => true }) }
  end

  class AcceptsHashWithKeys
    include Contract
    method :accepts => [ hash_with_keys(:foo, :bar) ]
    def initialize(options={}); end
  end

  def test_hash_with_keys_foo_bar
    assert_passes { AcceptsHashWithKeys.new({}) }
    assert_passes { AcceptsHashWithKeys.new({ :foo => "anything" }) }
    assert_passes { AcceptsHashWithKeys.new({ :bar => "anything" }) }
    assert_passes { AcceptsHashWithKeys.new({ :foo => "anything", :bar => "goes" }) }
    assert_violation { AcceptsHashWithKeys.new({ :arbitrary => "key" }) }
  end

  class AcceptsHashContract
    include Contract
    method :accepts => [ hash_contract({ :foo => String, :bar => Integer, :baz => Symbol }) ]
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

  class AcceptsRespondsTo
    include Contract
    method :accepts => [ responds_to?(:each, :first) ]
    def initialize(duck_array); end
  end

  def test_responds_to_each_first
    assert_violation { AcceptsRespondsTo.new({}) }
    assert_violation { AcceptsRespondsTo.new "foo" }
    assert_violation { AcceptsRespondsTo.new 3 }
    assert_passes    { AcceptsRespondsTo.new([]) }
  end

  class SimpleBeforeCondition
    include Contract
    before { false }
    def call_fails; end
    def call_passes; end
  end
  class ExtendsSimpleBeforeCondition < SimpleBeforeCondition; end

  def test_simple_before_condition
    assert_equal(1, SimpleBeforeCondition.checked_filters.length)
    assert_not_nil(SimpleBeforeCondition.checked_filters[:call_fails])
    assert_violation { SimpleBeforeCondition.new.call_fails }
    assert_passes    { SimpleBeforeCondition.new.call_passes }
    assert_equal(1, ExtendsSimpleBeforeCondition.checked_filters.length)
    assert_not_nil(ExtendsSimpleBeforeCondition.checked_filters[:call_fails])
    assert_violation { ExtendsSimpleBeforeCondition.new.call_fails }
    assert_passes    { ExtendsSimpleBeforeCondition.new.call_passes }
  end

  class SimpleAfterCondition
    include Contract
    after { |accepted, returned| returned }
    def call(bool); bool; end
  end

  def test_simple_after_condition
    assert_equal(1, SimpleAfterCondition.checked_filters.length)
    assert_not_nil(SimpleAfterCondition.checked_filters[:call])
    assert_violation { SimpleAfterCondition.new.call(false) }
    assert_violation { SimpleAfterCondition.new.call(nil)   }
    assert_passes    { SimpleAfterCondition.new.call(true)  }
    assert_passes    { SimpleAfterCondition.new.call("foo") }
  end

  class SimpleAroundCondition
    include Contract
    around {|arg| not arg}
    def call(bool); bool; end
  end

  def test_simple_around_condition
    [ 1, :foo, true, false, "bar", 8.3, nil ].each do |val|
      assert_violation { SimpleAroundCondition.new.call(val) }
    end
  end

  class ScopedBeforeCondition
    include Contract
    def initialize(bool); @bool = bool; end
    before { @bool }
    def call; end
  end

  def test_scoped_before_condition
    assert_violation { ScopedBeforeCondition.new(false).call }
    assert_passes    { ScopedBeforeCondition.new(true).call }
  end

end
