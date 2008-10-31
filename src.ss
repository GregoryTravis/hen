;(def main (/. () (if 'True 10 20)))

;; (def main
;;      (/. ()
;; ;         ((/. (a b c) (Hoo c b a)) 1 2 3)))
;; ((/. (a) ((/. (b c) (Hoo c b a)) 2 3)) 1)
;; ))

;; (def main
;;      (/. ()
;;          ((/. a 10) 12)))

;(def main (/. () 1))

;; (def poo (/. (a b) (Hoo b a)))
;; (def main (/. () (poo 1 2)))
;(def main (/. () ((/. (a b) (Hoo b a)) 1 2)))
(def main (/. () ((/. (1 b) (Hoo b b)) 1 2)))
