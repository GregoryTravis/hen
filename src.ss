(include "overture.ss")

(fun (foo a b) (bar b b))
(foo 10 20)
(bar 10 20)
(foo 20 10)
(bar 20 10)

(fun (tak ()) ())
(fun (tak (cons a d)) (cons (harf a a) (tak d)))
(tak [1 2])

(fun (yep a) (double a a))
(map yep [1 2])

(append [1 2] [3 4])

(reverse [1 2])
(reverse [1 2 3 4])

(reverse (append [1 2] [3 4]))

(+ 3 4)
