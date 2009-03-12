(def foo (/. (x) x))

(fun (foo2 x) x)
(fun (double x) (+ x x))

(fun (goop (Foo x)) (+ x x))

(fun (main)
     (list
      (list 10 (foo 20) (foo2 20) (double 30))
      (list
       ((/. x x boom) 10)
       ((/. (x . y) x boom) (cons 1 2))
       ((/. (x . y) y boom) (cons 1 2))
       ((/. ((x . y) . z) x boom) (cons (cons 1 2) 3))
       ((/. ((x . y) . z) y boom) (cons (cons 1 2) 3))
       ((/. ((x . y) . z) z boom) (cons (cons 1 2) 3))
       ((/. (x . (y . z)) x boom) (cons 1 (cons 2 3)))
       ((/. (x . (y . z)) y boom) (cons 1 (cons 2 3)))
       ((/. (x . (y . z)) z boom) (cons 1 (cons 2 3)))
       )
      (goop (Foo 10))))
