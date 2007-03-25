# test_handshake.rb
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

require 'test/unit'
require 'handshake'

class TestContract < Test::Unit::TestCase

  class InvariantDeclarations
    include Handshake
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
    include Handshake
    invariant { false }
  end

  def test_basic_invariant
    assert_violation { NonFunctionalArray.new }
  end

  class NonEmptyArray < Array
    include Handshake
    invariant { not empty? }
  end
  class ExtendsNonEmptyArray < NonEmptyArray; end
  
  class PositiveBalance
    include Handshake
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
    include Handshake
    contract :accepts_str, String => anything
    contract :accepts_int, Integer => anything
    def accepts_str(str); str; end
    def accepts_int(int); int; end
  end
  class ExtendsMethodDeclarations < MethodDeclarations; end

  def test_method_declarations
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

  def test_method_accepts
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
    contract String => anything
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
    include Handshake
    contract [ String, Integer ] => anything
    def call(arg1, arg2); return arg1, arg2; end
  end

  def test_method_returns_multiple
    assert_violation { ReturnsMultiple.new.call("foo", "foo") }
    assert_violation { ReturnsMultiple.new.call(3, 3) }
    assert_passes    { ReturnsMultiple.new.call("foo", 3) }
  end

  class AcceptsVarargs
    include Handshake
    contract [[ String ]] => anything
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
    include Handshake
    contract Block => anything
    def call1; end
    contract Block => anything
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

  class AcceptsWriter
    include Handshake
    contract String => anything
    def val=(str); @str = str; end
  end

  def test_writer_method_accepts_str
    assert_violation { AcceptsWriter.new.val = 3 }
    assert_violation { AcceptsWriter.new.val = :foo }
    assert_passes    { AcceptsWriter.new.val = "foo" }
  end

  # EXCELSIOR!
  class AcceptsMixed
    include Handshake
    contract [ String, String, [ Integer ], Block ] => String
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
    include Handshake
    equals_foo  = clause {|o| o == "foo"}
    contract [ equals_foo ] => anything
    def call(foo)
      return foo
    end
  end

  def test_method_simple_assertion
    assert_violation { AcceptsSimpleAssertion.new.call }
    assert_violation { AcceptsSimpleAssertion.new.call 3 }
    assert_violation { AcceptsSimpleAssertion.new.call "bar", "bar" }
    assert_passes    { AcceptsSimpleAssertion.new.call "foo" }
  end

  class AcceptsAll
    include Handshake
    equals_five = clause {|o| o == 5}
    contract all?(Integer, equals_five) => anything
    def initialize(n); end
  end

  def test_accepts_all_of
    assert_violation { AcceptsAll.new "foo" }
    assert_violation { AcceptsAll.new 3 }
    assert_violation { AcceptsAll.new 5.0 }
    assert_passes    { AcceptsAll.new 5 }
  end

  class AcceptsAny
    include Handshake
    equals_five = clause {|o| o == 5}
    equals_three = clause {|o| o == 3}
    contract any?(equals_five, equals_three) => anything
    def three_or_five(n); end
    contract any?(String, Integer, Symbol) => anything
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
    include Handshake
    contract not?(String) => anything
    def initialize(not_str); end
  end

  def test_accepts_not_string
    assert_violation { AcceptsNot.new "string" }
    assert_passes    { AcceptsNot.new 3 }
    assert_passes    { AcceptsNot.new :symbol }
  end

  class AcceptsBoolean
    include Handshake
    contract boolean? => anything
    def initialize(bool); end
  end

  def test_accepts_boolean
    assert_violation { AcceptsBoolean.new "foo" }
    assert_violation { AcceptsBoolean.new :foo }
    assert_passes    { AcceptsBoolean.new true }
    assert_passes    { AcceptsBoolean.new false }
  end

  class AcceptsNonzero
    include Handshake
    contract nonzero? => anything
    def initialize(nonzero); end
  end

  def test_accepts_nonzero
    assert_violation { AcceptsNonzero.new :foo }
    assert_violation { AcceptsNonzero.new 0 }
    assert_passes    { AcceptsNonzero.new 3 }
  end

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

  class AcceptsRespondsTo
    include Handshake
    contract responds_to?(:each, :first) => anything
    def initialize(duck_array); end
  end

  def test_responds_to_each_first
    assert_violation { AcceptsRespondsTo.new({}) }
    assert_violation { AcceptsRespondsTo.new "foo" }
    assert_violation { AcceptsRespondsTo.new 3 }
    assert_passes    { AcceptsRespondsTo.new([]) }
  end

  class AcceptsIsA
    include Handshake
    contract is?(:String) => is?(:Symbol)
    def call_is_a(str); return str.intern; end
  end

  def test_accepts_is_string_symbol
    assert_violation { AcceptsIsA.new.call_is_a(3) }
    assert_violation { AcceptsIsA.new.call_is_a(:foo) }
    assert_passes    { AcceptsIsA.new.call_is_a("foo") }
  end

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
    assert_equal(1, ExtendsSimpleBeforeCondition.method_contracts.length)
    assert_not_nil(ExtendsSimpleBeforeCondition.method_contracts[:call_fails])
    assert_violation { ExtendsSimpleBeforeCondition.new.call_fails }
    assert_passes    { ExtendsSimpleBeforeCondition.new.call_passes }
  end

  class SimpleAfterCondition
    include Handshake
    after { |accepted, returned| assert returned }
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

  class SimpleAroundCondition
    include Handshake
    around {|arg| assert(!arg) }
    def call(bool); bool; end
  end

  def test_simple_around_condition
    [ 1, :foo, true, false, "bar", 8.3, nil ].each do |val|
      assert_violation { SimpleAroundCondition.new.call(val) }
    end
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

  class BeforeClauseAssert
    include Handshake
    
    before do |arg|
      assert_equal("foo", arg, "arg must equal foo")
    end; def call(arg)
      arg
    end
  end

  def test_before_clause_assert
    assert_violation { BeforeClauseAssert.new.call 3 }
    assert_violation { BeforeClauseAssert.new.call "bar" }
    assert_passes    { BeforeClauseAssert.new.call "foo" }
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

  def test_accepts_super_and_sub
    assert_violation { AcceptsSuperAndSub.new.call 3 }
    assert_passes    { AcceptsSuperAndSub.new.call Superclass.new }
    assert_passes    { AcceptsSuperAndSub.new.call Subclass.new }
    assert_passes    { Superclass.new == Subclass.new }
  end
end
