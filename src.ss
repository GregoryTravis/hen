(fun (map f (Cons a d)) (Cons (f a) (map f d)))
(fun (map f Nil) Nil)

(fun (ulp-panties a) a)

;(fun (ponce x) 10)
(fun (id x) x)
(fun (doublerer f) (/. (x) (* 2 (f x))))

(fun (main) (Coot
             ((id id) 3)
             ((doublerer id) 4)
             (map (doublerer id) (Cons 1 (Cons 2 Nil)))
             joe
             joe
             (ulp-panties 1)
            ))
