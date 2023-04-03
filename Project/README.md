# Source

## pt.lua
A simplistic yet powerful library provided by the teachers to recursively print tables.

## utils.lua
Various utilitary tools loaded directly as global variables.

## parser.lua
The (lexer and) parser module.
Loads a function which parses the input code into an Abstract Syntax Tree (AST).

## compiler.lua
The compiler module.
Loads a function which compiles the AST into opcode.

## interpreter.lua
The interpreter module.
Loads a function which interprets the code using a stack machine of sort.

## test.lua
Executes a battery of mostly outdated tests.

## main.lua
Executes the input code passed in the standard input.
