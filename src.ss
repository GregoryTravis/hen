(fun (bar a) (list a a))
(bar 10)

(fun (foo a) (? 'true) (list a a))
(foo 20)

(fun (baz a) (? 'false) (list a a))
(baz 30)
