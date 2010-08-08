(fun (foo (Cons a (Barf c d 12))) (Joe d c a))
(fun (foo (Cons a (Tween t))) (Ack t t t))
(fun (bar a b) (Blech b a))

(fun (dubble a) (Dubbled a a))
(fun (mapp f (Cons a d)) (Cons (f a) (mapp f d)))
(fun (mapp f Nil) Nil)
(fun (aply f arg) (f arg))

(Done
 (foo (Cons 10 (Barf 20 30 12)))
 (bar 1 2)
 (mapp dubble (Cons 1 (Cons 2 Nil)))
 (aply dubble 66)
 ;(/. (x) 1)
 ((/. (x) 1) 4)
)
