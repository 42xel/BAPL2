# everything is expression
## DONE
- make everything an expression. <br>
it's already done, baring control structures which depend on blocks which depends on sequence
- make subexpressions subexpressions, at least in the comment and in your head.<br>
sub expressions are things like factor and terms, which, due to infix operator finickness, may stick to nearby sub expressions.<br>
Let's call their result formula.
    - add imply to the language.
- make io a reference
    - add input statement : @
    - Maybe rewrite print as @ =
- make sequences expressions, that evaluate to the last expresion (parens are friends here).
    - remark : sequences and expression use semi-colon and comma as sort of infix operator. So they should be hanled similarly to formula (though not necessarilly formula themselves)
- tackle blocks. Aren't they already free with litteral arrays and sequence ? Rque : `{exp}` is sort of a pointer, with a heavy syntax.
    - "free". Whatever
- tackle `if` and `while` : recommend using parenthesised sequences with semi-colons.
- assignments as a left value ? (useful for default value)
## TODO

- submit lesson 6 and start working on functions.
- make list an expression. List are comma separated expressions, yielding several values at once. Allows several returns.
    - a big issue is `a and 3 or (4, 6)` with sort of a type error on the stack, unless `(4,6)` is cerced to `4`. I now understand better why lua does it that way. That implies they're only useable in special contexts such as assignements and returns.
    - use register ?
    - coerce everything to list in some contexts ?
    - at the end of the day, list as primary value, and that's what we're talking about, is OO. And OO is better done with Objects rather than stack manipulation.
    So do use return statements.
    - list usage "restricted" to lhs, rhs, return statement ?, packing and unpacking statements.

# functions
## DONE
## TODO
- think about order body, param, do you really wanna reverse ? I guess I don't. After all lhs needs to be exectued first.
Issue is, it's harder to specify parameters processing inside the parametrization block.
Not really an issue though, most languages are like that.
    - If anything, you can compose functions. (TODO examples)
    - Besides with more functional/promising shenanigans, post processing is probably possible still, something like `{ \To \Number a }` or whatever.

- make functions : `id # = exp` : `id` is set to the function which yields `exp`. `id #` to call it. High prio. So no anonymous function ? not necessarilly. `(#= exp)` could be one syntax, remember, affectation also yields result.
- syntax for parameters ? ideally, parameters are "just" codeblock concatenation with context fusion. `block1 \ block2`.
    - so make that codeblock fusion with context fusion.
    - what if `exp` in `id # = exp` is not a block, just an expression ?
    - work with non function and non block as well.
    - `block_param \ block_function` to bind some named values in block_function from block_param. Allows rebinding, and some dynamical scoping of sort ?
    - mutably ? use `##` to have coroutines or be able to copy function ?
    - `block_param \ block_function #` to call it (parsed as `(block_param \ block_function) #`)
    - `\` mutating the function it opers on also useful for promises later on `block_function \ then`. To ponder. differnece between `#` and `$` could be there.
    - to have numbered parameters and to filter them (something weird in the binding needs to happen. It's really binding not fusing blocks I guess. To fuse you can use paren blocks):
    ```
    f #= parametrizationBlock \ bodyBlock ;
    parameters \ f #;
    ```
- make unpacking syntax ? 
    - `.{a, b, c} = {3, 4, 5}` ?
    - With optimization `{..a=3,  ..b = 4, ..c = 5}` ?
    - Allows to use block functions, which have scoping.
    - some lighter syntax, like `*{2,3} --> 2,3` (inspired by C pointers syntax) ? I'm not a fan of overloading `*`to be honest.
    How about `¤` instead ?
- varargs ?

# Object oriented programming
## DONE
## TODO
- goto and label
    - use promises obviously. But also namespace I guess.
    - first you can only jump within the same block of code ?
    - then you can jump out of block but not into ? with caution to stack management ?
        - Jumping out of function ? a priori, jump out of function call, with dynamical ctxtualisation, rather than jumping out of body function with static ctx. Otherwise the stack can't sensibly be handled.
    - then into block (code block or function ?), assuming no initialisation/binding needed ?
    - caution function vs coroutine. function to copy function/arrays ?
    - use them as statical fields rather than full dinamic dictionaries ? Kind of require to jump in the middle of a function from the outside, but some syntax could help. A wrapper of sort as well (can be implicit and automatically put at the beginning of the opCode of a function/block with labels)
    - jumping out of functions ? questions about label scoping. Most these questions will probably already be answered by variable scoping then, but there's still stack handling todo.
- leverage functional programming, use `\` to instanciate, access method, etc.
- strings, named fields ? static and dynamic ? For now, static should be manageable, through substitution.

# going further
## DONE
- What of control structure blocks (if, while) ? SHould we recommend writing them with parentheses (and semicolons) rather than brackets ? It doesn't change much tbh. If you're crazy enough need/want to to `a = if cond stat`, you should know what you need (paren block or unpacking). If you you just want regular if and while with scoping, nothing wrong with using brackets and not using the result. 
## TODO
- Promises
    - similar to function blocks but with substitution instead of binding, and callbacks instead of concatenation ?
    - how to make them disappear ? What's the equivalent of passing arguments by reference ? Do we need one different symbol per type of reference (types are at least functions, proactive promises and lazy promises) ?<br>
    Ideally, that symbol(s) is(are) only put in the `parametrizationBlock̀`. Maybe with binding ? Something ending with ` \\ (?a = a)` ? **Maybe passing by reference is actually the default behaviour and passing by value is the one requiring some work in the `parametrizationBlock̀`**. For named parameters, it could be as simple as `.a = a` to pass by value, versus just `(a)` to pass by reference. For numbered parameters, it pasisng by value could be the defalut, or there could be 2 operators, `\=` and `\.=`.
- context/clocks
    - `{ . = 5, 7}` or maybe `{ . ..= 5, 7}` for a block to set its own value, as a way to return. `.$ \ exp` to set a callback function. `.$ \ (.= exp)` or maybe even `.$ \,= exp` to set a return value

# Better error message
## DONE
## TODO

# Optimization
## DONE
## TODO
- control structure
    - nested elseifs may generate unnecessary jumps. Probably so do imply chains.
- optimizations like `0 + , 1 *, 0 * `,
    - absorbing elements
    - compile time computation of constantes.
    - exploiting commutativiy for the above.

# Misc.
## DONE
## TODO
- have chatGPT write tests for you.
- allow to make new falsy and truthy values with functions somehow ?
- repeat until and repeat unless more idiomatic ?
- replace keywords ?
- more default control structures and more flexible else ?
- storing varargs in closures with shortcuts ? the smartart thingy which does work in lua ?
```
Tuple = function(...)
     return function() return ... end
end

--usage
local t = Tuple(...)  -- packing equivalent
t()    --unpack equivalent
```
