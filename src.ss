(fun (map f Nil) Nil)
(fun (map f (Cons a d)) (Cons (f a) (map f d)))

(fun (foo x) (* x 2))

(map foo (Cons 1 (Cons 2 Nil)))
