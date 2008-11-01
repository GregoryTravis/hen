(def glap 133)

(def fact (/. (n) (if (== n 0)
                      1
                      (* n (fact (- n 1))))))

(def poo (/. (a b) (+ a b)))

(def proo (/. (a b) (Goo b a)))

(def main
     (/. ()
         (list (/. (x) 10)
               ((/. (x) 10) 1)
               ((/. (x) x) 1)
               ((/. (x) x) ((/. (x) x) 3))
               ((/. (x) glap) 1)
               1
               ((/. (x) x) 3)
               (+ glap 2)

               (if 'True 1 2)
               (if 'False 1 2)
               (> 1 2)
               (> 2 1)
               (== 0 0)
               (== 0 1)

               (fact 10)
               (list 1 2)
               ((/. () (poo ((/. () 1)) 2)))

               (proo 1 2)
               ((/. (a b) (Hoo b a)) 1 2)
               ((/. (1 b) (Joo b b)) 1 2)
               ((/. (a 2) (Zoo a a)) 1 2)
               ((/. ('jerk b) (Roo b b)) 'jerk 2)

               )))
