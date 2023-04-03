------name
just Strings (JS)

------expressions

expression => up to 3 kind of values : rvalue (right/regular), lvalue (left), gvalue (loGic, flow)

motivation : 
    a<b<c
    fuzzy =
    arg by ref
    sort(fns) : no longer have to choose.
    labels/goto
    table definition patterns
    destructuring patterns
    
------code bloc
code bloc/graph



------context
Contexts
table/map. prototypal inheritance.


functions calls are just context and code bloc
functions are classes whose calls are instances.
nice conceptually, but how inefficient?

macro would be just code bloc executed in current ctx

------spécial variables
_variables
manipulated like regular var but have special meaning.


------flow
as much as possible, flow operator behave like normal expressions

revertible?

goto
wentto
camefrom
comefrom

goto anywhere in the callstack, even virtual callee places
goto label, callstackPosition, environment

repeat // continue but doesn't skip condition, or does
continue
break
return

asyncreturn

not/or not
return myVar or not
//pb syntaxic lookahead?? or maybe compile of sort, eventhough I don't want to compile.

while
for
for( ;;break)

if
do
until
unless
else
actually
otherwise

btw : forces dynamic behaviour?
wlog : forces static behaviour ?

execution graph, can be modified dynamically, except in strict mode

tools for lazy eval

lazy (doesn't eval unless asked to) Promises  £
regular (always asynchronous) Promises €
proactive (synchrounous whenever possible) Promises  $
syntax similar to pointers in C (*). Op inverse or chained resolution could be § §§
Promiselike types
promise coercion : $id when id->nil, id<- == lazy unresolved
easy syntaxes for Promises, with assignement acting as resolution, operator overloads, functional promises, upvalue promises...

a€b or a.€b : then

make €f the default way to make recursion
and $f the default way to use old f whe' overriding it.

(lazy|regular|proactive) binding/vow : ££ $$ (like Promise but value is not locked upon resolution)
$4:100 : promise but only gets updated at depth 4 number 100

a = ££nil | a = €nil : weak ref of sort
(a = file.open("bla.txt")) = €nil : opens a file to be close by the next cycle. (alternative to with).


£return €return $return : non stopping (asynchronous) return prompting the corresponding value/promise to be returned at the end of the function
$£return $€return : breaking variant.
subsequent returns overrides each other.
$$return : would be equivalent to return. Instead, returns the last 

some sort of error handling, with catch and finally.
one way using no new syntax and (promissed) statements as expression :
          (£try=code)€finally $try$catch
One way using specific syntax : $try?catch:finally
where [?!:][£€$] are chaining, respectively error, success and unconditional, and $ or ! can be implied.

------natural language
very sketchy.
polysemy?
    super duper sketchy

coding case :
Type variable
Type Table = ...
table1, table_napkins : variables that are auyomagically typed as Table
Type To = Function

inheritance :
Rect Square = ...
Type Table = ...
type aliases :
Function To

prototyping/linking?
prototype object

dATE_NOW : global variable, of type Date

even better (no, but good challenge) : don't allow for anything other than alpha, so that variables of a same type can only be distinguished by case.


context prefix and implicit argument passing.

--the below is too verbose
coding camel case ?

to curedADiseasedCatOrSuchDogWithMeduccineTheKnowwledgeAndAHerbAndHerb_curePets_someDescriptiveName

    
    pet cat
    rub dog's tummy
...}
function curePets ( & object : cat | dog, medicine : knowmedge, someHerb1 : herb, someHerb2 : herb ) {
    if (!(isDiseased(object)) throw "error object"
}

raise

getMoneyFromAccount
getMoney[FromHere] 
    get est un verbe prédéfini, from est optionel et quand non donné, renvoie le context courrant, avec suffisement de boyaux pour faire de la POO.


------OOP
superPrototypal, like lua, but with more built in tools.

    
lookup concatenative language
    and point-free
