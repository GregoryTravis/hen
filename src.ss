(include "overture.ss")

;; (if (= 3 3) 1 2)
;; (if (> 3 3) 1 2)

(fun (fact n)
     (if (= n 0)
         1
         (* n (fact (- n 1)))))
(fact 10)
