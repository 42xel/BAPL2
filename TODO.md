# Comments
- Transform current `;` into `;;` (and `;;` into `;;;`)
- Use `;` or tabulation or break line as a statement separator. Notably breaking function calls.
    - `;` becomes inline again (kind of equivalent to C's comma)

# expressions
## DONE
- tackle `if` and `while` : recommend using parenthesised sequences.
- make list an expression. List are comma separated expressions, yielding several values at once. Allows several returns.
    - a big issue is `a and 3 or (4, 6)` with sort of a type error on the stack, unless `(4,6)` is cerced to `4`. I now understand better why lua does it that way. That implies they're only useable in special contexts such as assignements and returns.
    - use register ?
    - coerce everything to list in some contexts ?
    - at the end of the day, list as primary value, and that's what we're talking about, is OO. And OO is better done with Objects rather than stack manipulation.
    So do use return statements.
    - list usage "restricted" to lhs, rhs, return statement ?, packing and unpacking statements.
## TODO
- Make while yield a sensible value => the falsy value that broke the loop.
- Apply a precedence climbing algorithm to be more permissive with unary operators.
- Use space/no space to specify priority with preffix and suffix (with warnings).

# Assign Statements
## DONE
## TODO
- list assignement
- change assign such that right side is always evaluated first ?
    - code not executed in the order it's written though, but it's probably better that way.
    - code still compiled in the order it's written (notably for variable declaration)
    - should still be somewhat neutral on assignement overload.
- Revisit assignement as left value, keep default value in mind

# Type system
## DONE
## TODO
- Base types
    - int
    - float ?
    - bool ??
    - char ?
    - nil
- Blocks
    - Arrays : just **data**
    - Contexts : a set of **variables** with possibly a **stack**, which can be confused with **data**
    - Function : a callable **context** with a **function body**, for now can be confused with **data** but I may wanna change that
In addition, Blocks can be
- **static** :
    - **data** are in a constant size *array*
    - constant set of **variables** stored in a constant size *array*
    - **function body** is a non mutable array
- **dynamic**
    - **data** are in a variable size *vector*
    - **variables** are in a *dictionary* of sort
    - **function body** is a *vector*

- Composed types (for statical type) ? :
    - union types ? with union index for keys
    - fields themselves are typed (but not necessarilly mapped to a static type) ?
    - return value of functions are typed ?

- Type annotation :
    - using left association and prototype types :
    ```
    (Var = _Type) = val
    ((Var = _Type) = default) = val
    ```
    exploiting the fact that left associative assignement chains are compiled left to right but executed right to left.
    - doing something normal like using `:`.

# Variable Names
## DONE
## TODO
- Starting with an lower case : dynamically typed
- Starting with an upper case : statically typed
    - inferred from declarations, error on things like `A = b`.
- prefixed by an underscore : constant
    - May have several declaration, the first executed will replace the other.
    - In the functions code : `goto assign` and `goto val` which gets lazily replaced by `load v` once the value is set.

    starting with `_` : fresh variables, its scope is punctual or last exactly one expression (that is its parent node of the AST). Useful for anonymous function without having to open a block : `#_= ()`. Useful for singletons : `a = (.._ ??= {1,2,3})[b]` : `{1,2,3}` is only ever built once (per time its context is built), but you can write it exactly where you use it instead of where it needs to live, and you don't need to give it a name.
    - `_` itself has a somewhat special meaning : void/nil. As a value, 
- Ponder static/dynamic, stack/heap.
- Name mapping, inheritance :
    - Names are mapped to some arithmetic value `n` and types to some arithmetic value `t` such that some function `f` maps `(t,n)` to preferably unique prefeably contiguous named fields of the type.
    - If Tb extends Ta, that is if Tb can be seen as a Ta, we should have `f(tb,n) = f(ta,n)` for all fields of Ta appearing in Tb. (differentiates copy and expands, with copy not requiring these constraints).
        - more general notion of "is related" ? on a type basis or on a field basis ? Use Traits ? Non instantiated Types only meant to be expanded who don't care about having contiguous indices ?
    - Idea : `f(t,n) = 0` might mean `N` is not a field of `T`.
    - Idea : `n : {prime : smolInt, mod : smolInt}` and `t : longInt`, and `f(t,n) = t % n.prime == n.mod ? n.mod : 0`.
        - t is inferred from contained fields and chinese lemma.
        - if t becomes too big, switch `f(t,.)` to a perfect hashmap.
        - first solve the choice for all `n.mod`. You need it anyway (even for hashmap), you need only it for static.
        - related names (eg. names which may appear both appear in the same type) may not use the same prime.
        - unrelated names may use the same prime but ought to use different mod.
- do same for suffixes (`MyType1`, `MyType_a`, ...) ?

# staticism / dynamism
## DONE
## TODO
- dynamic staticism.
    - idempotent binary rewriting :
        - the binary is eventually stable.
        - the rewriting never changes the semantic.
    - used for compilation ?
- arbitrary binary introspection and rewriting.
    - limited to function code written on the heap ?
    - used for compilation ?
- Safe, statical, scoped transmutations. In-place metadata : possibility to leave some room on a struct to be borrowed as anything small enough.
    - it's like a dyn traits object, except instead of a vtable known at compile time, the trait itself can be first class, and dynamic. Essentially, it allows to change the type behaviour of a given variable at runtime, in a manner still controlled at compile time, without shadowing or enum or combining several datastuctue, and their potential cost of copy and indirection.
    ```rust
    #[derive(Default)]
    struct Sboobs  {
        foo: (),
        bar: InPlaceMeta<const Size = 29>,    //some space for in place metadata
    }
    let mut s : Sboobs = Default::default();
    let as_i64 : MetaLifetime<i16> = s.bar.init(47);  //l effectively creates a mutable reference to s.bar. 47 is written in the 16 bits of s.bar
    println!("{}, s.bar.get(&as_i64)");   //you have to specify the lifetime, to help the compiler (and programmer) with types and lifetimes.
    s.bar.set(&mut as_i64, 57);

    // let l2 : MetaLifetime<i16> = s.bar.init(54);  //would error as s.bar is effectively borrowed mutably
    // you can however borrow the inner value, which effectively borrows the lifetime this time
    let alias = s.bar.borrow(&as_i64);
    let alias = s.bar.borrow_mut(&mut as_i64);
    drop(alias);

    //you should be able to call all the method directly on the lifetime as well, since it is doing the heavy lifting, but it's less logical.
    // With some compiler modifications, the following syntax would be sweet :
    s.bar<'as_i64> = 49;

    drop(a);
    // when a is dropped, control of s.bar is released
    let b : &mut [bool; 2] = s.bar.init([true, false]);

    b.leak();  // consumes b instead of dropping it. a.bar is released, but the data is left untouched, b is moved rather than dropped, for future, presumably unsafe uses. It is a leak, as no valid variable points to data which haven't been properly dropped.
    ```

# functions
## DONE
    - block evaluation :
        - end of block / break : self (self[0])
        - return : nil
        - return exp : exp (potentially list)
        - so return statement writes in caller/parent ?
    - for later : dizionario ? (woudn't it be easier right away ?)
## TODO
- Syntax : drop the `#`
- function as proxies for their calls set to the defaut parameters environment ?

- [old] Syntax for parameters ? ideally, parameters are "just" codeblock concatenation with context fusion. `block1 \ block2`.
    - so make that codeblock fusion with context fusion.
    - `block_param \ block_function` to bind some named values in block_function from block_param. Allows rebinding, and some dynamical scoping of sort ?
    - mutably ? use `##` to have coroutines or be able to copy function ?
    - `block_param \ block_function #` to call it (parsed as `(block_param \ block_function) #`)
    - `\` mutating the function it opers on also useful for promises later on `block_function \ then`. To ponder. differnece between `#` and `$` could be there.
    - to have numbered parameters and to filter them (something weird in the binding needs to happen. It's really binding not fusing blocks I guess. To fuse you can use paren blocks):
    ```
    f #= parametrizationBlock \ bodyBlock ;
    parameters \ f #;
    ```

- add `...̀  which represent whatever is on the stack and use it to pass parameters, explicitely while chaining blocks, implicitely when using parentheses for functions calls.
- make unpacking syntax ? or list assignement ?
    - `.{a, b, c} = {3, 4, 5}` ?
    - With optimization `{..a=3,  ..b = 4, ..c = 5}` ?
    - Allows to use block functions, which have scoping.
    - some lighter syntax, like `*{2,3} --> 2,3` (inspired by C pointers syntax) ? I'm not a fan of overloading `*`to be honest.
    How about `¤` instead ?
- varargs ?
- flatten the code and use only gotos ? there are pros and con, but mostly no.
    - With flatten, I'd have to handle the callstack entirely myself. That's something I wat I guess, but also, no.
- think about tail recursivity.

# Memory management
## DONE
## TODO
- Ownership system like in Rust.
- when a context is closed without being assigned, it gets cleared, after its content
- when a variable is changed : clear previous value.
- can only write things like `a = B` if `a` is owned, directly or indirectly, by `B`'s owner.
- ownership transfer ? not absolutely needed.
- borrow ? `a _= B` meaning (see function) set `a` to the function which retrieve B, taking no argument and applying automatically.


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
## TODO
- Promises
    - similar to function blocks but with substitution instead of binding, and callbacks instead of concatenation ?
    - how to make them disappear ? What's the equivalent of passing arguments by reference ? Do we need one different symbol per type of reference (types are at least functions, proactive promises and lazy promises) ?<br>
    Ideally, that symbol(s) is(are) only put in the `parametrizationBlock̀`. Maybe with binding ? Something ending with ` \\ (?a = a)` ? **Maybe passing by reference is actually the default behaviour and passing by value is the one requiring some work in the `parametrizationBlock̀`**. For named parameters, it could be as simple as `.a = a` to pass by value, versus just `(a)` to pass by reference. For numbered parameters, it pasisng by value could be the defalut, or there could be 2 operators, `\=` and `\.=`.

# Better error message
## DONE
## TODO
- give warning when an expresion could be complete at the end of the line but actually continues on th next one.
- give line number for dynamical error too
    - store more metadata in the AST (and around the opCode).

# Optimization
## DONE
## TODO
- control structure
    - nested elseifs may generate unnecessary jumps. Probably so do imply chains.
- optimizations like `0 + , 1 *, 0 * `,
    - absorbing elements
    - compile time computation of constantes.
    - exploiting commutativiy for the above.
- tail recursivity
- consecutive clean
    - more generally, the stack is filled with garbage and I'd be very surprised to not have let any way to access it. Both are issues to look into

# Description
## DONE
## TODO
- Talk about new goal : read as written. No (necessity for) forward declaration, no necessity for breaking the flow of a current logic chained ran concurrently to another one.
- make a first example with say a hash table inside a loop.
    - with other languages you should put it beforehand, but then it hurts readibility. You can put it a header file, but it does not change much. With dynamic languages, you can actually put it afterward, but it's still the same deal
    - with FeAsKo, you can write it where you use it in the code and still have it defined where you need it in the memory. Not only that, but for a one time value defined using static constant, the amortized cost is not even that of a reference call, but that of a hard writen constant.
- Think about the name of the language. Could be something about telling stories, Bard is already taken, could be menestrel.

# Misc.
## DONE
## TODO
- have chatGPT write tests for you.
- allow to make new falsy and truthy values with functions somehow ?
- repeat until and repeat unless, more idiomatic ?
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
