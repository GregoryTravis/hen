(fun (foo (type tctor alts))
     tctor)
(fun (bar (type tctor alts))
     alts)

(foo (type (A b c) (list (B b b) (C b c))))
(bar (type (A b c) (list (B b b) (C b c))))
