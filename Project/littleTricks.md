
# No capture pattern : `* Cc(nil)`
Sometimes, we have patterns which return a variable number of captures and may return none, such a sequences of statements.
For example :
```
patt = lpeg.C(lpeg.R('az', 'AZ'))^0
```
When a pattern returns no capture, the position after the whole match is returned instead. It is problematic if the desired behaviour is to actually produce captures.

**A trick I've came up with is to append a constant nil capture.**

For example :

```
lpeg.Ct(patt * lpeg.Cc(nil)):match""
```
will return an empty table (where `lpeg.Ct(patt):match""` would return `{1}̀`).

It also works for most function, as the value of an argument not provided is `nil` as well.

It is worth noting that a `nil` capture is still a cpture (as opposed to an absence of capture). For example, 
```
(lpeg.Cc(nil) / print):match""
```
will print nil, and 
```
(lpeg.Cc'#' * lpeg.Cc(nil)^-43 / select):match""
```
will evaluate as 43

# Single pass multipass.

Using promises (callback functions) allows to do things in a single ass that would otherwise require several.

Things like gathering names (function, variable, labels ...), crossed recursion.