# Parsing Ripper output with Megaparsec.

This is just a scratch pad for playing with and understanding the Haskell library [Megaparsec](https://hackage.haskell.org/package/megaparsec). To do so, I tried to write a parser for Ruby's Ripper s-expression output. [Ripper](http://ruby-doc.org/stdlib-2.0.0/libdoc/ripper/rdoc/Ripper.html) let's you get s-expressions for Ruby code like so:

``` irb
>> require 'ripper'
>> require 'pp'
>> Ripper.sexp('def hello; puts "hey"; end')
=> [:program, [[:def, [:@ident, "hello", [1, 4]], [:params, nil, nil, nil, nil, nil, nil, nil], [:bodystmt, [[:command, [:@ident, "puts", [1, 11]], [:args_add_block, [[:string_literal, [:string_content, [:@tstring_content, "hey", [1, 17]]]]], false]]], nil, nil, nil]]]]
```

Or alternatively, pipe in anything form stdin:

``` sh
echo 'def hello; puts "hey"; end' | ruby -r ripper -r pp -e "pp Ripper.sexp(STDIN.read)"
```

This is the input to the [parser](https://github.com/tclem/megaparsec-play/blob/master/app/Main.hs).
