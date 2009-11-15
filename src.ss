(fun (map f (Cons a d)) (Cons (f a) (map f d)))
(fun (map f Nil) Nil)
(fun (dbl x) (+ x x))

(fun (main) (Coot
             (map dbl (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
             ))
