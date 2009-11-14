(fun (map f (Cons a d)) (Cons (f a) (map f d)))
(fun (map f Nil) Nil)

(fun (ulp-panties a) a)

;(fun (ponce x) 10)
(fun (id x) x)
(fun (doublerer f) (/. (x) (* 2 (f x))))

(fun (fact 0) 1)
(fun (fact n) (* n (fact (- n 1))))

(fun (length (Cons a d)) (+ 1 (length d)))
(fun (length Nil) 0)

(fun (ntimes 0 thunk) Nil)
(fun (ntimes n thunk) (Cons (thunk) (ntimes (- n 1) thunk)))

(fun (main) (Coot
             ((id id) 3)
             ((doublerer id) 4)
             (map (doublerer id) (Cons 1 (Cons 2 Nil)))
             joe
             joe
             (ulp-panties 1)
             (fact 10)
             (length (Cons 1 (Cons 2 Nil)))
             (length (ntimes 10 (/. () (fact 10))))
             ((/. () 5))
             ((/. (x) (x)) (/. () 11))
             ;(ntimes 10 (/. () (fact 10)))
            ))
