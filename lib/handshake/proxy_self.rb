# Redefines each of the given methods as a call to self#send.  This assumes
# that self#send knows what do with them.  In this case is to make sure each
# method call is checked by contracts.
class Class # :nodoc:
  def proxy_self(*meths)
    meths.each do |meth|
      class_eval <<-EOS
        def #{meth}(*args, &block)
          self.send(:#{meth}, *args, &block)
        end
      EOS
    end
    nil
  end

  # def ===(other)
  #   other.is_a? self
  # end
end
