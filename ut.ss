(load "h.ss")

(ut
 (map preprocess-pattern
      '(a
        Foo
        (Foo a b)
        (Foo a (Bar b))))
 '((var a)
   (const Foo)
   (pair (const Foo) (pair (var a) (pair (var b) (nil))))
   (pair
    (const Foo)
    (pair (var a) (pair (pair (const Bar) (pair (var b) (nil))) (nil))))))
