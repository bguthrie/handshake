module Handshake
  class CheckedProc < Proc
    attr_accessor :contract

    def initialize(contract, &block)
      @contract = contract
      super(&block)
    end

    def checked?
      not @contract.nil?
    end
  
    def call(*args)
      Handshake.catch_contract("Contract violated in call to proc #{self}") do
        @contract.check_accepts!(*args)
      end if checked?
      
      return_value = super(*args)
      
      Handshake.catch_contract("Contract violated by proc #{self}") do
        @contract.check_returns! return_value
      end if checked?
      
      return_value
    end
  end

  # For block-checking, we need a class which is_a? Proc for instance checking
  # purposes but isn't the same so as not to prevent the user from passing in
  # explicitly defined procs as arguments.
  # Retained for backwards compatibility; all blocks should be block contracts.
  class Block
    def Block.===(o); Proc === o; end
  end
  
  module ClauseMethods
    
    # Block signature definition.  Returns a ProcContract with the given
    # attributes
    def Block(contract_hash)
      pc = Handshake::ProcContract.new
      pc.signature = contract_hash
      pc
    end
    
  end
end
