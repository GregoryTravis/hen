(fun (main) (Coot
            (joe 10 20)
            ((/. (x) x) 5)
            ((/. (f) (f 5)) (/. (p) p))
            (map (/. (x) (+ x x)) (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
            (map (/. (x) (* x x)) (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
            ((/. (x) (* x x)) 11)
            ))
