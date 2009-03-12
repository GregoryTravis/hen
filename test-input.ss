(def foo (/. (x) x))

(fun (foo2 x) x)
(fun (double x) (+ x x))

(fun (main)
     (list 10 (foo 20) (foo2 20) (double 30)))
