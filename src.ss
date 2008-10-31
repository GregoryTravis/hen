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
;; (def main (/. () (integer-+ a 2)))

(def main (/. () (if 'True 1 2)))
(def main (/. () (if 'False 1 2)))
(def main (/. () (> 1 2)))
(def main (/. () (> 2 1)))
