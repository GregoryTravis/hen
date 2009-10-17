(fun (apply f arg) (f arg))
(fun (dbl x) (+ x x))

(fun (main) (Coot
             (apply dbl 10)))
