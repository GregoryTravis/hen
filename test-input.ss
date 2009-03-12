(def foo (/. (x) x))

(fun (foo2 x) x)
(fun (double x) (+ x x))

(fun (main)
     (cons 10 (cons (foo 20) (cons (foo2 20) (double 30)))))
