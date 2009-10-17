(fun (apply f arg) (f arg))
(fun (dbl x) (+ x x))

(fun (rebuild (Cons a d)) (Cons a (rebuild d)))
(fun (rebuild Nil) Nil)

(fun (map f (Cons a d)) (Cons (f a) (map f d)))
(fun (map f Nil) Nil)

(fun (main) (Coot
             (apply dbl 10)
             (rebuild (Cons 1 (Cons 2 (Cons 3 Nil))))
             (map dbl (Cons 1 (Cons 2 (Cons 3 Nil))))))
