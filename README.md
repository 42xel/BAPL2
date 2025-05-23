# Final Project Report: deFAKto

## Preamble
The code source is in the folder Project.
All the files there baring lpeg.so and pt.lua have been entirely written by myself.

### Usage
lua version 5.4.2 or above is recommended. If I remember correctly 5.3 or above is neessary because of changes in file opening syntaxe.

Only tested on linux, I have no idea how it plays with Windows filenames for example.

All commands are to be run from the `Project/` folder, and the filenames and foldernames to be given from there as well.

| name | command(s) | description |
|-|-|-|
| main | `lua main.lua <filename>` | Parses, compiles and runs the designated file as interpreted in the language. In the standard output, it prints : the content of the file, the parsed AST, the compiled opcode, the content of the file again. It then runs the program which may interact with both the default input and output. Lastly, it prints the stack as it is at the end of the script execution. |
| input | `lua main.lua ../input` | Main applied to the input program. The input program solves the 8 queens problem while demonstrating many specificities of the language. |
| test | `lua tests/test.lua <filename>` <br> `lua tests/test.lua <foldername>/` | Executes (calls main on) one or several scripts specified by a motif, decorating the outputs with some additional informations (script name). It waits for the user to press enter between each script. |

## Language Syntax

### Numerals

Numbers are formed roughly the same as in lua : they can be specified in decimal and hexadecimal (prefixed by `0X`), integer or float.
Decimals can also be written in scientific notation (with a whole exponent) using the letter `e`.
Case does not matter.

example of numbers :
```
36
0XAB5B.c3
74.98e78
```

### Basic Expressions

In deFAKto, most things are expressions.
The basic expressions (arithmetico-logical expressions) are formed by aggregating patterns of decreasing precedence corresponding each to operators and operations.

Here is a table of supported operators in decreasing order of precedence :

| operator | meaning | associativity | remark |
|----------|---------|---------------|--------|
| `^` | power | right |
| `+` <br> `-` | unary plus <br> unary minus | unary | unary operators may be chained but there must be space between two consecutive unary operators
| `//` <br> `*` <br> `/` <br> `%` | floor division <br> multiplication <br> float division <br> modulo | left |
| `+` <br> `-` | plus <br> minus | left | left associativity ensures `a+b-c+d` is correctly parsed as `((a+b)-c)+d`
| `!!` | bitwise not | unary | 
| `>>`  <br> `<<` | right shift <br> left shift | left |
| `&&` | bitwise and | left |
| `~~` | bitwise xor | left |
| `\|\|` | bitwise or | left |
| `<` <br> `>` <br> `<=` <br> `>=` <br> `==` <br> `!=` | lesser than <br> greater than <br> l. or equal <br> g. or equal <br> equal <br> different | special, left, lazy | `a < b < c` means, `a < b` and `b < c`, but `b` is evaluated only once. Comparison operators can be freely mixed and matched.
| `!` | not | unary |
| `&` | and | right, lazy |
| `\|` | or | right, lazy |
| `=>` | imply | right, lazy | The logical imply operator. `a => b` is semantically equivalent to `!a \| b`
| `=` | assignement | right associative | an assignment evaluates as the value of its right hand side |

Parentheses allow total control over grouping and priorities.

### Sequences

A program in deFAKto is merely a sequence of expressions to be evaluated.
The program runs from the beginning of the file (no special entry point such as a `main` function).

Expressions are simply separated by spaces, but can also, optionally, be separated by a semicolon at the end of a line.

For example,
```
0xAB 5-7 34 << 3
```
is a valid deFAKto program, athough it's highly recommended to write it :
```
0xAB ;
5-7 ;
34 << 3 ;
```

### Comments and Semicolons

In addition to serving as optional (and sometimes mandatory) separators, semicolons are used for comments.

* A semicolon as the first character of a line is the start of a pure comment.
* A semicolon after an expression, on the same line, is an expression separator and the start of a comment
* A semicolon preceded only by space on its line is undefined behaviour, meaning I know what it does and want it the other way.

In a sense, semicolons as separators have the lowest priority, they separate only fully formed expressions.

Comments are delimited as follow :

* A semicolon immediately followed by a sequence of consecutive opening braces `{` is the start of a **group comment**, which last until a sequence of consecutive closing braces `}` in equal number.
* A semicolon followed by no opening brace is the start of a **line comment**, which lasts until the end of the line.

For example, `7-3`,
```
7-
3
```
```
7
-3
```
```
7 - 
;the proper way to comment in the middle of an expression
3
```
are all equivalent programs. Whereas this :
```
7 - ;
3
```
will cause an error since `7-` is not a valid expression. On the other hand, that :
```
7 ;
-3
```
will cut the expression in two, so as to be equivalent to `7 (-3)`

### Control Flow
`If` statements are formed as followed :
`if cond exp1`, and with an `else` :
`if cond exp1 else exp2`

There is no `elseif`, but `else if`achieves the same goal through nesting ̀`if` statements without having to materialize the nesting in the code.

For example, the following code
```
if a%2
    if a != 1
        3 * 1 + a
    else
        a // 2
```
 is equivalent to
```
if a%2
    (if a != 1
        3 * 1 + a
    else
        a // 2)
```
and if we want it the other way, we have to write :
```
if a%2
    (if a != 1
        3 * 1 + a)
else
    a // 2
```

You'll observe that the "then" and "else" bodies are expressions.
They are not formula computed in vain, they are actually used to evaluate the if statement, which is itself an expression.
In fact an `if else` statement in deFAKto is exactly a ternary operator.
The snippet :
```
r = (if a 2*a else b)
```
means sensibly the same as, say, the following JavaScript snippet :
```js
r = a ? 2*a : b ;
```
Without `else`, `if cond exp` is equivalent to `cond & exp`.

The syntax for `while` follows the same spirit :
`while cond exp` except its evaluation is unspecified (yet).

### Types
deFAKto is dynamically and very weakly typed.

The four current types are :

* numbers
* arrays (which are tables in disguise)
* functions (which are arrays in disguise)
* nil values, represented by lua nil values. They are generally not supported baring being printed in an array, and should not be used.

For logical operators, the number `0` is false, and everything else is considered true.
When we can't be lazy about the truthy value an expression evaluates to, such as `!0`, `1` is used to denote true.
The value `nil` is not falsy, and you shouldn't use it.

Did I mention that you shouldn't use nil values ? because you shouldn't.
If you still want to use `nil`, as you shouldn't, two current reliable ways to get a `nil` value are :
`{.=()}` (see the *stack introspection* subsection of section *new feature / change*)
and
`(new[1])[1]`   (see the *Array* subsection)

The language cruelly lacks type safeguards at the moment and relies entirely on lua.

Equality test of Array and Functions is to be understood as these object being the same, on the same physical location of memory.

### Array
A simple array is declared with the following syntax : 
`new [size]`
Such an array is empty ("filled" with nil values, as much as a lua table can be).
The size of the array is checked when trying to get and set values and an error is thrown when the index is out of bound.

```
t = new [3] ;
t [2] = 5 ;
@ = t ;
```

An array can be initialized with the following syntax :
`new [size] = init`
This equal sign has a special meaning but should follow the same priority and associativity rules as in assignements (in case of doubt/bug, use parentheses).
The initializing expression is played for each cell of the array to fill.
for example, `t = new [2] = new[3] = 0` will create a 2 by 3 array filled with zeroes and will assign it to `t`.
In particular, the 2 rows of `t`, although created the same way, are distinct and changing one won't alter the other.

Chaining the brackets offers a shortcut to create multidimensional arrays, with similar syntax for indexing :
```
a = new [2][2] = 1 ;
a [0][1] = 0 ;
a [1][0] = 0 ;
```
will create the array
```
{
    { 0, 1 },
    { 1, 0 }
}
```
The syntax `t[a][b]...` is parsed as : `(t[a])[b]...`
and the syntax `new [a][b]... = init`
is syntactic sugar for 
`new [a] = new [b]... = init`

### Variables

An **identifier**, or variable name, is a sequence of alphanumerical characters not starting with a digit.

A variable is an optional prefix followed by an identifier.
The prefix dictates the nature of the variable.

* A `~` prefix denotes a global variable.
* A sequence of dots `.` denotes a local variable. See the section *Blocks* for more details. `.` means here, in the current scope, `..` means in the parent scope, `...` in the grand-parent scope and so on.
* A sequence of question marks `?` denotes a parameter. See the section **Functions** for more details. Very very roughly, `?` means the caller, `??` the caller's caller and so on.
* Without any prefix, the nature of a variable is inferred at compile time.

Variable need to appear declared before used in the code.
For that they need to be the left hand side of an assignement.
They don't have to actually be initialized, a hacky piece of code such as `0 => (a = 0)` will satisfy the compiler.

Resolving the nature and scope of a prefixless variable is done as follows :
- we look for the nearest parameters fitting the name first
- then the nearest fitting local variable
- last we look in the global variables space

If no suiting variable is found, a global variable is created if the variable was the left hand side of an assignement, and an error is thrown otherwise.

### IO variable

As a special token behaving like a variable, `@` will, as a left hand side of an assignement, print the expression intead of assigning it.
As a value, it will prompt the user to enter a value, which will replace it.

### List
Another way to separate expressions is using comma.
It does more than that actually, it pushes all the values to the stack while pointing at the first one.
A trailing comma is accepted

```
a = 5, 6,
```
Will assign `5` to `a`.

Comma have a higher priority than `=`and a lower priority than anything else.

Currently, the only intended use of list is within blocks.
In particular, list as left hand side, multiple assignements and multiple value returns are not yet supported.

### Blocks and Scoping
Blocks are braces-delimited pieces of code, with two special effect :
* Reguarding variables, they create a child scope.
* They create a new stack on which expressions are evaluated

At the end of a block, said block evaluates to itself, that is as an array filled with the values left on its stack.

Concretely, blocks can serve as **litteral arrays** :
```
{1, 2, 3}
```
but also
```
{1, {4, 5, 6}, new [3] = 2}
```
and even
```
{
    .a = 5 ;
    .b = 3 ;
    {..c = 4};
    while a = a - 1
        b = 2 * b ;
    b, c
}
```
(yields `{48, 4}`)
etc.

A variable may also consist in solely a prefix, without a name.
In that case, it refers to the context itself, or more exactly to its self-referring variable.

For example, the following code will print 1 :
```
.a = {..b = .}
@ = a == b
```
The followwing code :
```
@ = {.=3 ;
    4,5,6}
```
will not break, the `.` is only a variable referring by default to the context itself but it can be modified without affecting the block itself.
What it does however, is affect the value yielded, which here will be `3`, and not `{4,5,6}`.

For now, the following codes errors :
```
@ = {.[3] = 5 }
```
Because within itself, the block is considered an array of length zero.
It's only once it closes while yielding itself that it is interpreted as a litteral array and gets granted a non zero length.

### Functions

The syntax for functions is as follows `# f arg` with a parameter, and `# f ;` or `(# f)` without one.
The semicolon or parentheses are not part of the function per see but I can't recommend them enough to avoid sticking to the next expression.

This pattern has lower precedence over array indexation, meaning `#a[1]` means `#(a[1])`.

To declare a function, use a function pattern as a left side of an assignement.
For example, this
```
~a = 4 ;
# .f = ~a = ~a + 1;
# f;
# f;
```
defines a function which increments the global variable `a` and yields its value, stores this function in a local variable `f`, then calls it twice.

The logic behind the notation is loosely inspired from C pointers :
`#a = exp` sets `a` to the value such that `#a` yields `exp`.

For now, simple functions can have only a single anonymous parameter (there are non simple functions ahead), referred to as `?` (the self-refering variable of the calling context).

For example, the function square is written :
```
# square = ? * ?;
@ = # square 7 ;    prints 49
```

Of course, one possible way to specify several inputs and to have several output is to use arrays :
```
# idiv = {?[1] // ?[2], ?[1] % ?[2]};
@ = # idiv {17, 3} ; prints {5, 2}
```
There is no unpacking syntax helping destructuring arrays.

A parameter can also be used in a function declaration to specify a default value.
For example :
```
# double 2 = ? + ?;
@ = # double ;      prints 4
@ = # double 6 ;    prints 12
```

Because functions are first class values, and the language is dynamically typed, recursion is both easy and non existant per say, following sort of a global declaration paradigm for recursion and forward declaration.
Indeed, in the following code,
```
# pow2 = if ? 2 * #pow2 (?-1) else 1 ;
@ = # pow2 5 ;
```
`pow2` does not call it self, rather, it calls the function which is called `pow2`.
The difference can be examplified by the following code :
```
.f = # pow2 = if ? 2 * #pow2 (?-1) else 1 ;
# pow2 = 7 ;
@ = # f 5 ; prints 14 rather than 32
```

A point worthy of mention is that forward declaration is not necessary for simple reccursion.
In the above examples, `f` and `pow2` can already be used in the rhs of an assignement whose lhs is a function pattern where they appear.

For crossed recursion, the program (the compiler) just needs to know the variable names exist at the moment of using them in the declaration of the functions.

The same way one can use pointers of pointers in C/C++, there are function patterns of function patterns in deFAKto, denoted :
```
## f p1 p2
```
It is syntactic sugar for 
```
#(# f p1) p2
```
The above syntax may be used both as lhs and rhs, and a left hand side, 
`#(# f p1) p2 = exp` means :
```
# f p1 = (# _  p2 = exp)
```
without the border effect of setting the value of `_`.
That is `f` is the function which returns a function which returns `exp`.

For example :
```
# add 0 = (# _  10 = ?? + ?) ;
@ = ## add 3 9 ; 12
@ = ## add () 9 ; 9
@ = ## add 3 () ; 13
```
Defines the addition, with zero as the first default value and 10 as the second.
`?` has the same meaning as before.
`??` refers to the outer parameter, when looking at it as nested functions.

And other way to conceive it is `?` refers to the last parameters, whereas `??` refers to the inner parameter of the chained function definition.
```
#(# pow 1) = ? ^ ??;        chained definition, parentheses are optional
@ = #(# pow ) 12 ; 12
@ = ## pow 2 5 ; 25
```

This not only gives a somewhat flatter way to provide functions with several parameters, it allows *curryfication* of sort :
```
## .pow 1 = ? ^ ??;
.id = #pow          ;the identity function
.square = #pow 2    ;the square function
.cube = #pow 3      ;the cube function
```

Oh and by the way, there is a *return* statement.
It is what you expect.
Return statements are not mandatory.
In contrast with C or lua and in likeliness of OCaml and Rust, lack of a return statements does not mean no returned value: by default the body of a function is evaluated as an expression.

So return statements are best used to exit the body of a function early and to return falsy.

## New Features/Changes

### A remark on pattern priority and preffix patterns

I added a couple prefixed unary operators (plus, minus, not, but also arguably functions).
An important limitation currently is that we still have to use parentheses according to operator precedence even when the prefixed nature of the pattern makes it unambiguous.

For example, 
```
; unary minus function, using 2-complement
# .minus = 1 + !! ?
```
will yield a syntax error, because `+` has a higher precedence than `!!`.
By contrast, the equivalent lua code
```lua
--unary minus function, using 2-complement
local function minus(a) return 1 + ~ a end
```
is interpreted just fine.

In deFAKto, for now at least, we have to write :
```
# .minus = 1 + (!! ?)
```

A similar issue is the precedence between indexation and function call.
```
# range = {1, 2, 3, 4, 5, 6, 7, 8, 9,}
; @ = #range[7] ; errors, as it means #(range[7])
; @ = (#range)[7] ; the current shortest correct syntax.
; @ = #range,[7] ; a desired idiomatic syntax
@ = (#range,)[7]
```
The desired syntax `@ = #range,[7]` errors because lists can't be indexed, because it would require them to have higher precedence over indexation rather than lower.

I don't know if there is a purely PEG solution to this issue, but presumably, precedence climbing could solve it.

### Dynamism (indexed and called expressions)
Indexed and function pattern are not limited to variables, they can be used with any expression, which hopefully resolves into a value of correct type at runtime.

Example 1 :
```
{..t =. ;
0, 0, 0, } [2] = 5;
@ = t ; {0, 5, 0}
```

Example 2 :
```
; a mini calculator. It asks for a binary operation, then for its two operands.
@ = ## {
; 1 for add
    ( ##._= ?? + ? ),
; 2 for minus
    ( ##._= ?? - ? ),
; 3 for multiplication
    ( ##._= ?? * ? ),
; 4 for division
    ( ##._= ?? / ? ),
}[@] @ @

```

### Assignements and order of execution

So far, we've seen that assignements were expressions, which can be right associatively chained.

We've also seen that left hand side could be :
* *a variable*, to assign it.
* *an indexed* expression, to set the value of an array cell.
* *a function pattern* to declare a function.

As a small extra sweet, the left hand side can also be an assignement.
`(@ = 5) = 3`
means `@=5 @=3`

One sizeable issue with enticing users to exotic syntax, other than producing unleggible code, is that the order in which left hand side and right hand side and their side effect are evaluated is unspecified.
Specifying it is a question no matter what, but it becomes more visible as fiddling is encouraged.
For now, the order of evaluation has always been taken for convenience of implementation, and does not follow any logic or consistency.

### Imply operator

The right associative imply operator allows for interesting control flow.
```
.succes = !(treat1 => treat2 => treat3 => 0)
```
It allows to chain treatements that depend on each other.
Because it evaluates as true when its left hand side is falsy, some workaround is necessary to produce sensible output, namely finishing the sequence by 0 so that it evaluates to a different logical value than an early failure.

It can also, on its own, replace `if else`, and more generally chains of `else if` :
```
    (cond1 => exp1 => 0)
=>  (cond2 => exp2 => 0)
=>  (cond3 => exp3 => 0)
; ...
```

Example :
```
SUCCESS = 0

n = @
# Syracuse = (
;prints n and if it is 1 stops
    ((@ = n) == 1 => SUCCESS)
;elseif n is odd, multiply it by 3, add 1 and call Syracuse
    => ( n % 2 => (n = 3 * n + 1) => # Syracuse)
;else half n and call Syracuse
    => ( (n = n // 2) => # Syracuse)
);

# Syracuse
```

### Iterator Array Initializer
Because in the initialization of `new[size] = exp` the expression is played once per cell, it allows us to use iterators to initialise arrays.

For example :
```
# range = new [?, (? = ? + 1) ] = (? = ? - 1)
@ = #range 10
```
the function range above creates arrays of a size specified by the parameters, and filled with consecutive integers from 1 to the size of the array.
It also handily relies on the list element are evaluated and what value the whole list yields.
Lists can be used to store a value in their first element and do some post treatement before using the value.
The idiomatic equivalent of C++'s `C++` is thus `(C, C = C + 1)`.

A major issue here is the order in which the expression is player.
For now it is played for the last index first, out of convenience for me.
Moving forward, it will require thoughts and specifications.

An other use case of array initializer is to make (albeit somewhat expensively) for loops (whose sized is known in advance) :
```
@ = {
    .= 0
    new[10, .i = 1] = (
        new[10, .j = 2] = (
            ( .= . + i * j ), (j = j + 1)
        ),
        ( i = i + 1 )
    )
}
```

### Scoping
Although standard nowadays, scoping is a fascinating problem and a powerful feature.
At the moment, a lot of orginal features seem to work properly, such as `..a` to refer to and possibly declare a local variable in a parent context.

Local variables are statically scoped, meaning functions referring to local variables refer to the variables where they have been defined rather than called. (that's the usual behaviour for popular language nowadays).

The local variables a block will get are defined at compiletime. At runtime, their lifetime is the same as their block. For example :
```
.a = 3
#f = a
.a = 5
@ = # f
```
will print 5. The second `.a` does not define a new local variable, it sets the only local variable `a` the current context will ever have.
By contrast, in lua, 
```lua
local a = 3
function f () return a end
local a = 5
print (f())
```
will print 3.

I haven't seen any major bug related to scoping, however, many things haven't been properly tested yet.

### Blocks and functions
Blocks and functions have great synergy together, this subsection is dedicated to it.

First it's important to mention that functions (or `if`, or `while`) by default do not create a new scope.
New scopes are created exactly by braces delimited blocks.
The one exception to this rule is the whole program itself, which also is its own toplevel scope (that's what enables writing local variable at top level).

deFAKto lacks a dedicated syntax for anonymous functions, but the following idiom covers that need :
```
idendtity = {#.= ?}
```
Here, `{#.= ?}` opens a block, defines a function without polluting any namespace, and yields the function as a value.
Keep in mind that, in order to define new local variables in parent contexts, you'll then need one more dot than if you declare the function's body without a block (if you're only using existing or global variables and are not using any prefixes, it shouldn't change anything).

As expected, encapsulating a function within a block allows to have persistent local variables, for example :
```
# mem = {
    .nil = ();
    .val = ?;
    #.= if ? == nil val else val = ?
}
a = # mem 3;
@ = #a;     3
@ = #a 7;   7
@ = #a;     7
```
One thing special about deFAKto is that such encapsulation doesn't require forward declaration to have border effects :
```
{
    .nil = ();
    .val = 3 ;
    . = # ..a = (
    if ? == nil val else val = ?
)}
```
Both context evaluation (`.=`) and parent context lookup (`..a`) provide a way to initiate effects outside of the local scope, for better or worse.
The equivalent lua code for example would be :
```lua
local a --forward declaration
(function()
    local val = 3
    a = function (x)
        if x ~= nil then
            val = x
         end
         return val
    end
    return a
end) ()
```

It also allows to make true recursive functions, by making the referrence to self unaccessible from the outside (without exploits).

If we take a look back at our previous example :
```
.f = # pow2 = if ? 2 * #pow2 (?-1) else 1 ;
# pow2 = 7 ;
@ = # f 5 ; prints 14 rather than 32
```
and rewrite it using a block :
```
.f = pow2 = {#. = if ? 2 * #. (?-1) else 1} ;
# pow2 = 7 ; we can no longer change what the recursive function referes to
@ = # f 5 ; 32
```
we can no longer break the reference of the function to itself.

The syntax `#f = { ;{...} }` allows to declare function with each call instance having their own scope and local variables, in a way more in line with mainstream programming languages.

### Stack Introspection and other exploits

The behaviours described in this section are unintended, unsafe, unspecified and unreliable.

They are however a lot of fun and provide inspiration for the future.
They have to be fixed, but part of them may become features.

*Stack introspection* has accidentally been made possible, as a result of me being lazy when it comes to cleaning the stack (only changing the pointer without erasing the content).
Combined with the fact that most things are expressions that are evaluated and pushed to the stack even when not necessarilly used, along with some bridges otherwise leading to nowhere, it leads to intriguing phenomenons :
```
45, 52;
@ = a = ();         prints 45
@ = b = 0 + ();     print 52

72;
@ = c = {.=()}  ;   nil. 
;Some things, such as closing a block, force the cleansing of the stack.
;The opening of a block itself results in a fresh stack
;Supposedly, function returns also clean the stack
```
It may open some hacks possibility to code functions with multiple returns.

#### Function inspections and code injection :
```
.nil = ()

#f = 5 ;

@= f
@= f[2] ;nil
@ = #f ; 5

f[2] = 63

@= f
@= f[2] ; 63
@ = #f;  63

f[2] = nil

@= f
@= f[2] ; nil
@ = #f;  5
```
Internally, the static body of a function is a akin to an array of its instructions with some fluff.
Here, the array corresponding to `f` is :
`{'write', 5, 'ret'}`.

Because function values are only proxies of a static body with some fluff (the same static body may for example correspond to several function values, possibly each with differing default param values), and because of how array inspection works, one cannot inspect the static body of a function at runtime.
Attempting to do so will yield nil or error depending on the index and size of the function body.

It is however possible to inject code into the proxy and shadow the static body, which is what is happening in the code shown above.

## Future

The main gaping holes which would absolutely need to be filled before thinking about production are
* better framework
    * automatic tests.
    * better interface, command line for now. Options to only parse, only compile, run a precompiled code ... Better ways to specify input, ways to specify outputs.
    * much better error and warning reports, including type checking
* Fixing things in the interpreter
    * sanitize the stack
    * clarify how much functions (and more generally context) introspection and alteration is permitted and act accordingly.
* New Language features
    * iterators
    * clarify the value yielded by while
    * break statements, similar to return but for blocks
    * strings
    * handling nil values properly

The above would be necessary to make an appealing prototype.

For production, :
* framework
    * think about portability and packaging.
    * some documentation
* Core
    * pondering about memory allocation
    * Rewrite the virtual machine in Java/Kotlin | Rust | C/C++/D, mostly depending on choices for memory allocation.
    As a register machine ideally.
    * precedence climbing.
    * ponder transpilation into a popular language ??
* New Language features
    * named parameters
    * tuple assignement and/or destructuring pattern if deemed relevant.
    * some sort of context manipulation, ways to dynamically reopen blocks.
    It opens question about variable mapping, dictionary or not, etc.
    * some sort of minimal prototypal OO ? Ideally leveraging context, functional programming and introspection.
    * Files manipulation

For the very far future
* tail recursivity
* Ponder and improve assignements, order of evaluations...
* Promises
* online playground
* browser integration
* switch cases

## Self assessment

| Criteria | Score | Explanation |
|-|-|-|
| **Language Completeness** | 3 | All exercices treated, if some exotically, as well as two of the proposed feature : ternary operator and bitwise operators (challenging). Numerous self imposed challenge also taken, mainly first class functions and scopes. |
| **Code Quality and Report** | 2 | I think I barely tick every boxes of "meet expectations" on this one, but in a way very lacklusting and hindering, considering the size of the project, a 1 wouldn't surprise me. |
| **Originality and Scope** | 3 | I went above and beyond, and surely way too far for my own good. Regarding modularity, the proposed extra features were easily added as an afterthought, despite one being considered challenging. |

## References

Wikipedia
