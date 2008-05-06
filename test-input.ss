(fun (ack a b) (+ a b))

(+ 1 2)

(ack 20 34)

(+ 1 (+ 2 3))

(> 10 20)
(< 10 20)
(>= 10 20)
(<= 10 20)
(== 10 20)
(!= 10 20)
(if (== 10 10) 20 30)

(fun (fact 0) 1)
(fun (fact n)
     (if (>= n 1)
         (* n (fact (- n 1)))
         1))
(fact 10)

(read-objects "read-input.ss")

'(1 2)
