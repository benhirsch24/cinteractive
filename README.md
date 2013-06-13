## C Interactive Interpreter

Goal is to have a website where you can input a C program and get an interactive experience stepping through the program seeing the state of the heap and environment at every step.

Right now the C parser is powered by [language-c](http://hackage.haskell.org/package/language-c) and is using the [Warp Web Server](http://hackage.haskell.org/package/warp). The frontend will likely be mostly jQuery probably with some underscore or lodash with Bootstrap to make it look not horrible.

To build: cabal install
To run:   ./Server

To use a specific port set the PORT environment variable, otherwise it defaults to port 3000.

Haskell packages:

* text
* enumerator
* warp
* aeson
* utf8-string
* language-c (happy, alex which are a Haskell parser generator and lexer respectively are also required)
