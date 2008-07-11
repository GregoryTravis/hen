(fun (foo a b) (bar b b))
(foo 10 20)
(bar 10 20)
(foo 20 10)
(bar 20 10)

(fun (tak ()) ())
(fun (tak (cons a d)) (cons (harf a a) (tak d)))
(tak '(1 2))

(fun (yep a) (double a a))
(fun (map f ()) ())
(fun (map f (cons a d)) (cons (f a) (map f d)))
(map yep '(1 2))

(fun (append () a) a)
(fun (append (cons a d) b) (cons a (append d b)))
(append '(1 2) '(3 4))

(fun (reverse ()) ())
(fun (reverse (cons a d))
     (append (reverse d) (cons a ())))
(reverse '(1 2))
(reverse '(1 2 3 4))

(reverse (append '(1 2) '(3 4)))
