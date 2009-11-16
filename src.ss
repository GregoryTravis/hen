(fun (main) (Coot
            (map dbl (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
            (map dbl ($ 1 2 3 4 5 6))
            ($ 1 2 ($ 3 4) 5 6)
            ($ 3 4)
            (map dbl ($ 1))
            (map dbl ($))
            ($ 1 2)
             ))
