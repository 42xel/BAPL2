repo :
https://github.com/42xel/BAPL2.git

# Source

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

# Annotations
I was using lua-language-server for Vscode, as well as the annotations they support.
https://github.com/LuaLS/lua-language-server

