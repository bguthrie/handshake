require 'test/unit'
require 'handshake'

# The purpose of this test case is to ensure that when Handshake is suppressed
# contracts may still be declared without raising a syntax error, although
# they are not enforced.
class TestDisableHandshake < Test::Unit::TestCase

  def setup
    Handshake.suppress!
  end

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

  def test_handshake_is_suppressed
    assert Handshake.suppressed?
  end

  def test_contracts_not_enforced
    assert_nothing_raised { ComprehensiveContracts.new }
    assert_nothing_raised { ComprehensiveContracts.new.call 3 }
  end

end
