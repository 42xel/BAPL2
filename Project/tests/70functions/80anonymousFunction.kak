;; TODO anonymous function (and assignements statements) : `#= body` `#,param = body`
;; For now, the workaround `{#.=body}` should work, with caution given to the difference in scoping the block implies.
;; Another thing which should work without scoping issues is `#(##_=body)`, or even just `(#_=body)`.

