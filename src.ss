(fun (append Nil a) a)
(fun (append (Cons a d) b) (Cons a (append d b)))

(fun (reverse Nil) Nil)
(fun (reverse (Cons a d))
     (append (reverse d) (Cons a Nil)))

(reverse Nil)
(reverse (Cons 1 Nil))
(reverse (Cons 1 (Cons 2 Nil)))
(reverse (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
