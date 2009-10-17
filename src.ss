(fun (apply f arg) (f arg))
(fun (dbl x) (+ x x))

(fun (rebuild (Cons a d)) (Cons a (rebuild d)))
(fun (rebuild Nil) Nil)

(fun (map f (Cons a d)) (Cons (f a) (map f d)))
(fun (map f Nil) Nil)

(fun (fold f e (Cons a d)) (f a (fold f e d)))
(fun (fold f e Nil) e)

(fun (main) (Coot
             (apply dbl 10)
             (rebuild (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
             (map dbl (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
             (fold + 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
             (fold * 1 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))))
