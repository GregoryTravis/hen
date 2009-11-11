;(fun (ponce x) 10)
(fun (id x) x)
(fun (doublerer f) (/. (x) (* 2 (f x))))
(fun (main) (Coot
             ((id id) 3)
             ((doublerer id) 4)
            ))
