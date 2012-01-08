module Handshake
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
