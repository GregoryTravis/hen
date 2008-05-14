;(fun (add a b) (+ a b))

(fun (a-fun (+ a b)) 100)
(fun (a-fun c) 200)

(macro (a-macro (+ a b)) 100)
(macro (a-macro c) 200)

(a-fun (+ 1 2))
(a-macro (+ 1 2))
