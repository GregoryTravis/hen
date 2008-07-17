(include "overture.ss")

(fun (fact n)
     (if (= n 0)
         1
         (* n (fact (- n 1)))))
(fact 10)

(fun (fact2 0) 1)
(fun (fact2 n)
     (* n (fact2 (- n 1))))
(fact2 10)

[1 2]
