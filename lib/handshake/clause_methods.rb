module Handshake
  # Transforms the given block into a contract clause.  Clause fails if
  # the given block returns false or nil, passes otherwise.  See
  # Handshake::ClauseMethods for more examples of its use.  This object may
  # be instantiated directly but calling Handshake::ClauseMethods#clause is
  # generally preferable.
  class Clause
    # Defines a new Clause object with a block and a message.
    # The block should return a boolean value.  The message is optional but
    # strongly recommended for human-readable contract violation errors.
    def initialize(mesg=nil, &block) # :yields: argument
      @mesg, @block = mesg, block
    end
    # Returns true if the block passed to the constructor returns true when
    # called with the given argument.
    def ===(o)
      @block.call(o)
    end
    # Returns the message defined for this Clause, or "undocumented clause"
    # if none is defined.
    def inspect; @mesg || "undocumented clause"; end
    def ==(other)
      other.class == self.class && other.mesg == @mesg && other.block == @block
    end
  end
  
  # A collection of methods for defining constraints on method arguments.
  # Use them inline with method signatures:
  #   contract any?(String, nil) => all?(Fixnum, nonzero?)
  module ClauseMethods
    ANYTHING = Clause.new("anything") { true }

    # Passes if the given block returns true when passed the argument.
    def clause(mesg=nil, &block) # :yields: argument
      Clause.new(mesg, &block)
    end
    
    # Passes if the subclause does not pass on the argument.
    def not?(clause)
      clause("not #{clause.inspect}") { |o| not ( clause === o ) }
    end
    
    # Always passes.
    def anything; ANYTHING; end
    
    # Distinct from nil, and only for accepts: passes if zero arguments.
    # Since Ruby will throw an arity exception anyway, this is essentially
    # aesthetics.
    def nothing; []; end

    # Passes if argument is true or false.
    #   contract self => boolean?
    #   def ==(other)
    #     ...
    #   end
    def boolean?
      any?(TrueClass, FalseClass)
    end

    # Passes if any of the subclauses pass on the argument.
    #   contract any?(String, Symbol) => anything
    def any?(*clauses)
      clause("any of #{clauses.inspect}") { |o| clauses.any? {|c| c === o} }
    end
    alias :or? :any?
    
    # Passes only if all of the subclauses pass on the argument.
    #   contract all?(Integer, nonzero?)
    def all?(*clauses)
      clause("all of #{clauses.inspect}") { |o| clauses.all? {|c| c === o} }
    end
    alias :and? :all?
    
    # Passes if argument is numeric and nonzero.
    def nonzero?
      all? Numeric, clause("nonzero") {|o| o != 0}
    end

    # Passes if argument is Enumerable and the subclause passes on all of 
    # its objects.
    #
    #   class StringArray < Array
    #     include Handshake
    #     contract :+, many?(String) => self
    #   end
    def many?(clause)
      many_with_map?(clause) { |o| o }
    end
    
    # Passes if argument is Enumerable and the subclause passes on all of
    # its objects, mapped over the given block.
    #   contract many_with_map?(nonzero?, "person age") { |person| person.age } => anything
    def many_with_map?(clause, mesg=nil, &block) # :yields: argument
      map_mesg = ( mesg.nil? ? "" : " after map #{mesg}" )
      many_with_map = clause("many of #{clause.inspect}#{map_mesg}") do |o|
        o.map(&block).all? { |p| clause === p }
      end
      all? Enumerable, many_with_map
    end
    
    # Passes if argument is a Hash and if the key and value clauses pass all
    # of its keys and values, respectively.
    # E.g. <tt>hash_of?(Symbol, String)</tt>:
    #   
    #   :foo => "bar", :baz => "qux" # passes
    #   :foo => "bar", "baz" => 3    # fails
    def hash_of?(key_clause, value_clause)
      all_keys   = many_with_map?(key_clause, "all keys")     { |kv| kv[0] }
      all_values = many_with_map?(value_clause, "all values") { |kv| kv[1] }
      all? Hash, all_keys, all_values
    end

    # Passes only if argument is a hash and does not contain any keys except
    # those given.
    # E.g. <tt>hash_with_keys(:foo, :bar, :baz)</tt>:
    # 
    #   :foo => 3                                     # passes
    #   :foo => 10, :bar => "foo"                     # passes
    #   :foo => "eight", :chunky_bacon => "delicious" # fails
    def hash_with_keys(*keys)
      key_assertion = clause("contains keys #{keys.inspect}") do |o|
        ( o.keys - keys ).empty?
      end
      all? Hash, key_assertion
    end
    alias :hash_with_options :hash_with_keys

    # Passes if:
    # * argument is a hash, and
    # * argument contains only the keys explicitly specified in the given
    #   hash, and
    # * every value contract in the given hash passes every applicable value
    #   in the argument hash
    # E.g. <tt>hash_contract(:foo => String, :bar => Integer)</tt>
    #
    #   :foo => "foo"               # passes
    #   :bar => 3                   # passes
    #   :foo => "bar", :bar => 42   # passes
    #   :foo => 88, :bar => "none"  # fails
    def hash_contract(hash)
      value_assertions = hash.keys.map do |k|
        clause("key #{k} requires #{hash[k].inspect}") do |o|
          o.has_key?(k) ? hash[k] === o[k] : true
        end
      end
      all? hash_with_keys(*hash.keys), *value_assertions
    end

    # Passes if argument responds to all of the given methods.
    def responds_to?(*methods)
      respond_assertions = methods.map do |m| 
        clause("responds to #{m}") { |o| o.respond_to? m }
      end
      all?(*respond_assertions)
    end
    
    # Allows you to check whether the argument is_a? of the given symbol.
    # For example, is?(:String).  Useful for situations where you want
    # to check for a class type that hasn't been defined yet when Ruby
    # evaluates the contract but will have been by the time the code runs.
    # Note that <tt>String => anything</tt> is equivalent to
    # <tt>is?(:String) => anything</tt>.
    def is?(class_symbol)
      clause(class_symbol.to_s) { |o|
        Object.const_defined?(class_symbol) && o.is_a?(Object.const_get(class_symbol))
      }
    end

  end
end
