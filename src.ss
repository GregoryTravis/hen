(fun (grep p Nil) Nil)
(fun (grep p (Cons a d))
     (if (p a)
         (Cons a (grep p d))
         (grep p d)))

(fun (glap 1) True)
(fun (glap x) False)

(grep glap (Cons 1 (Cons 2 (Cons 3 (Cons 1 Nil)))))
