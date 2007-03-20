
class Class
  def hijack(method, &block)
    orig_method = "orig_#{method}".to_sym
    instance_eval do
      alias orig_method method 
      define_method(method, &block)
    end
  end
end

