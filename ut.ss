(load "h.ss")

(ut
 (map preprocess-pattern
      '(a
        Foo
        (Foo a b)
        (Foo a (Bar b))))
 '((var a)
   (const Foo)
   (pair (const Foo) (pair (var a) (pair (var b) (const Nil))))
   (pair
    (const Foo)
    (pair (var a) (pair (pair (const Bar) (pair (var b) (const Nil))) (const Nil)))))

 (map
  (lambda (x)
    (mtch x
          (pattern e) (match-pattern (preprocess-pattern pattern) (preprocess-pattern e))))
  (scoop-by 2
            '(
              Nil Nil
              a 1
              a Nil
              (Foo a b) (Foo 1 2)
              (Foo a b) (Foo (Bar 2 1) (Baz 4 3))
              )))

 '((just ())
   (just ((a (const 1))))
   (just ((a (const Nil))))
   (just ((a (const 1)) (b (const 2))))
   (just
    ((a (pair (const Bar) (pair (const 2) (pair (const 1) (const Nil)))))
     (b (pair (const Baz) (pair (const 4) (pair (const 3) (const Nil))))))))
)
