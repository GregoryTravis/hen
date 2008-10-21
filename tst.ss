(load "h.ss")

(shew
 (syn 'a)
 (syn '(a 10 b 20))
 (syn ''a)

 (unsyn (syn 'a))
 (unsyn (syn '(a 10 b 20)))
 (unsyn (syn ''a))

 (car (moch '(pair (atom a) (pair (atom 10) (pair (var b) (pair (atom 20) (atom ())))))
            '(pair (atom a) (pair (atom 10) (pair (atom 400) (pair (atom 20) (atom ())))))))
 (rw (syn '(a 10 b 20)) '(pair (atom bleah) (pair (var b) (atom ()))) (syn '(a 10 400 20)))

 (apply-binding-to-body-var 'a '((a (atom 1))) '(var a))
 (apply-binding-to-body-var 'a '((b (atom 2)) (a (atom 1)) (c (atom 3))) '(var a))

 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) '(var a))
 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) (syn 'a))

 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) '(var b))
 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) (syn 'b))

 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) '(var c))
 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) (syn 'c))

 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) '(pair (var c) (atom ())))
 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) (syn '(c)))

 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) '(pair (var c) (pair (var b) (atom ()))))
 (apply-bindings '((b (atom 2)) (a (atom 1)) (c (atom 3))) (syn '(c b)))


 (rw '(var a) '(var a) '(atom 10))
 (rw (syn 'a) (syn 'a) (syn 10))
 (rw (syn '(a b c)) (syn '(d c b)) (syn '(a 2 3)))

 (car (rw (syn '(a (Foo b) (Bar c))) (syn '(Baz c b)) (syn '(a (Foo 2) (Bar (Cup 3))))))
 (syn '(Baz (Cup 3) 2))

 (moch (syn '(Foo a)) (syn '(Bar 10)))
 (rw (syn '(Foo a)) (syn '(Ook a a)) (syn '(Bar 10)))

 (rwrw (list
        (list (syn '(Foo a)) (syn '(Ook a a)))
        (list (syn '(Bar a)) (syn '(Ack a a))))
       (syn '(Bar 10)))

 (moch `(var a) 20)
 (moch (syn 'a) 20)

 (moch `(pair (atom a) (pair (var b) (pair (atom 20) (atom ())))) `(pair (atom a) (pair (atom 2) (pair (atom 20) (atom ())))))
 (moch (syn `(a b 20)) (syn `(a 2 20)))

 (moch '(pair (atom a) (pair (var b) (pair (var c) (atom ())))) '(pair (atom a) (pair (atom 1) (pair (atom 2) (atom ())))))
 (moch (syn '(a b c)) (syn '(a 1 2)))

 (moch '(atom 3) '(atom 3))
 (moch (syn 3) (syn 3))

 (moch '(var jerk) '(var jerk))
 (moch (syn 'jerk) (syn 'jerk))

 (moch `(pair (atom Foo) (pair (var a) (atom ())))
       `(pair (atom Foo) (pair (atom 10) (atom ()))))
 (moch (syn '(Foo a)) (syn '(Foo 10)))

 (unsyn
  (car (rwrw (list
              (list (syn '(Foo a)) (syn '(Ook a a)))
              (list (syn '(Bar a)) (syn '(Ack a a))))
             (syn '(Bar 10)))))
 )

(shew (run (sb-read-file "tst-input.ss")))
