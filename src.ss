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

(def poo (/. (a b) (Goo b a)))
(def main
     (/. ()
         (list
;;           (poo 1 2)
;;           ((/. (a b) (Hoo b a)) 1 2)
;;           ((/. (1 b) (Joo b b)) 1 2)
;;           ((/. (a 2) (Zoo a a)) 1 2)
;;           ((/. ('jerk b) (Roo b b)) 'jerk 2)
;          (list ((/. (1) 2) 1))
          (list ((/. (10) 2) 1))
          )))
;(def main (/. () ((/. ('jerk2 b) (Hoo b b)) 'jerk 2)))
