# Handshake

Handshake is an informal AOP and [design-by-contract][dbc] system written in pure Ruby.
It's intended to allow Ruby developers to apply simple, clear constraints
to their methods and classes.

[dbc]: http://en.wikipedia.org/wiki/Design_by_contract

### Features

  * Method signature contracts
  * Contracts on blocks and procs
  * Method pre- and post-conditions
  * Class invariants

### Examples

Here's an example of Handshake in action on a hypothetical `BankAccount` class:

```ruby
  class BankAccount
    attr_reader :balance
    
    class << self
      def less_than_balance?
        all? positive_number?, clause {|n| n <= balance}
      end
    end
    
    invariant { balance >= 0 }
    
    contract positive_number? => anything
    def initialize(balance)
      @balance = balance
    end
    
    contract less_than_balance? => positive_number?
    def withdraw(amount)
      new_balance = @balance - amount
      @balance = new_balance
      return new_balance
    end
  end
```

Here's an example that uses an invariant to enforce a constraint on a subclass of Array:

```ruby
  class NonEmptyArray < Array
    include Handshake
    invariant { not empty? }
  end
```

Further specializing the subclass:

```ruby
  class NonEmptyStringArray < NonEmptyArray
    contract :initialize, [[ String ]] => anything
    contract :<<, String => self
    contract :+, many?(String) => self
    contract :each, Block(String => anything) => self
  end
```

Handshake can also define pre- and post-conditions on your methods.

```ruby
  class Foo
    before do
      assert( not @widget.nil? )
    end
    def something_that_requires_widget
      ...
    end
  end
```

See `Handshake::ClassMethods` for more documentation on exact syntax and
capabilities.  `Handshake::ClauseMethods` contains a number of helper and
combinator clauses for defining contract signatures.

### Caveats

Handshake works by wrapping any class that includes it with a proxy object
that performs the relevant contract checks.  It acts as a barrier between
an object and its callers.  Unfortunately, this means that internal calls,
for example to private methods, that do not pass across this barrier, are
unchecked.  Here's an example:

```ruby
  class UncheckedCall
    include Handshake

    contract String => Numeric
    def checked_public(str); str.to_i; end

    def checked_public_delegates(str)
      checked_private(str)
    end

    private
    contract String => Numeric
    def checked_private(str); str.to_i; end
  end
```

In this example, we have a public checked method protected by a contract.  Any
external call to this method will be checked.  The method marked as
`checked_public_delegates` calls a private method that is itself protected by a
contract.  But because the call to that private method is internal, and does not
pass across the contract barrier, no contract will be applied.

You can get around this problem by calling private methods on the special
private method `checked_self`:

```ruby
  class UncheckedCall
    ...
    def checked_public_delegates(str)
      checked_self.checked_private(str)
    end
    ...
  end
```

### License (MIT)

Copyright (c) 2010 Brian Guthrie

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
