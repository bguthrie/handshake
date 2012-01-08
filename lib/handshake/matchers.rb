RSpec::Matchers.define :violate_contract do |expected|
  match do |actual|
    actual.should raise_error(Handshake::ContractViolation)
  end
end