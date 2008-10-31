;; (/. (x) 10)
;; ((/. (x) 10) 1)
;; ((/. (x) x) 1)
;; ((/. (x) x) ((/. (x) x) 3))
;; ((/. (x) y) 1)
;(/. () 1)
;(/. () ((/. (x) x) 3))

;(def main (/. () 1))
;(def main (/. () ((/. (x) x) 3)))

;; (def a 10)
;; (def main (/. () (+ a 2)))

(def main (/. () (if 'True 1 2)))
(def main (/. () (if 'False 1 2)))
(def main (/. () (> 1 2)))
(def main (/. () (> 2 1)))
(def main (/. () (== 0 0)))
(def main (/. () (== 0 1)))

(def fact (/. (n) (if (== n 0)
                      1
                      (* n (fact (- n 1))))))
(def main (/. () (fact 10)))
