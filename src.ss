(def ones
  (/./.
   (/. (P a d) ((cons a) (ones d)))
   (/. x x)))
(ones (P 1 (P 2 (P 3 Nil))))

(def double (/. x (+ x x)))
(double 10)

(def map
     (/. f
         (/./.
          (/. (P a d) ((cons (f a)) ((map f) d)))
          (/. x x))))
(map double (P 1 (P 2 (P 3 Nil))))
