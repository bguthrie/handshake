require 'test/unit'
require 'contract'

class TestMethodContract < Test::Unit::TestCase
  
  class DefinesContract
    include Handshake
    contract :initialize, String => anything
    contract Integer => Integer
    def call(n); n * 4; end
  end
  class ExtendsDefinesContract < DefinesContract
    contract :blammo, Numeric => Symbol
  end

  def test_method_contract_initialize
    assert_equal(2, DefinesContract.method_contracts.size)
    assert_not_nil(DefinesContract.method_contracts[:initialize])
    assert_not_nil(DefinesContract.method_contracts[:call])
    assert_equal([String], DefinesContract.method_contracts[:initialize].accepts)
    assert_not_nil(DefinesContract.method_contracts[:initialize])
  end
end
