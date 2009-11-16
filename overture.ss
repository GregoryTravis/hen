(fun (id x) x)

(fun (not True) False)
(fun (not False) True)

(fun (fact 0) 1)
(fun (fact n) (* n (fact (- n 1))))

(fun (apply f arg) (f arg))

(fun (dbl x) (+ x x))

(fun (rebuild (Cons a d)) (Cons a (rebuild d)))
(fun (rebuild Nil) Nil)

(fun (map f (Cons a d)) (Cons (f a) (map f d)))
(fun (map f Nil) Nil)

(fun (fold f e (Cons a d)) (f a (fold f e d)))
(fun (fold f e Nil) e)

(fun (length (Cons a d)) (+ 1 (length d)))
(fun (length Nil) 0)

(fun (ntimes 0 thunk) Nil)
(fun (ntimes n thunk) (Cons (thunk) (ntimes (- n 1) thunk)))
