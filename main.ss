(load "h.ss")

(map preprocess-pattern
     '(a
       Foo
       (Foo a b)
       (Foo a (Bar b))))
