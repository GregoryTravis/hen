(fun (append Nil a) a)
(fun (append (Cons a d) b) (Cons a (append d b)))

(append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)))
(append Nil (Cons 3 (Cons 4 Nil)))
(append (Cons 3 (Cons 4 Nil)) Nil)
