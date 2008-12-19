(def ones
  (/./.
   (/. ((PP aaa d)) (cons (PP 1 aaa) (ones d)))
   (/. (x) x)))
(ones Nil)
(ones (PP 1 Nil))
(ones (PP 1 (PP 2 (PP 3 Nil))))
