(fun (map f ()) ())
(fun (map f (cons a d)) (cons (f a) (map f d)))

(fun (append () a) a)
(fun (append (cons a d) b) (cons a (append d b)))

(fun (reverse ()) ())
(fun (reverse (cons a d))
     (append (reverse d) (cons a ())))
