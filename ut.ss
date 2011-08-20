(load "h.ss")

(ut
 (map preprocess-pattern
      '(a
        Foo
        (Foo a b)
        (Foo a (Bar b))))
 '((var a)
   (ctor Foo)
   (pair (ctor Foo) (pair (var a) (pair (var b) (nil))))
   (pair
    (ctor Foo)
    (pair (var a) (pair (pair (ctor Bar) (pair (var b) (nil))) (nil))))))
