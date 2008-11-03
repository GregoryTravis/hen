;; (def fact2
;;      (/./.
;;       (/. (0) 1)
;;       (/. (n) (* n (fact2 (- n 1))))))
;; (def main
;;      (/. () (fact2 10)))

(def fact2 (/./. (/. (0) 1) (/. (n) (* n (fact2 (- n 1))))))
(fun (fact3 0) 1)
(fun (fact3 n) (* n (fact3 (- n 1))))

(fun (main) (list (fact2 10) (fact3 10)))
