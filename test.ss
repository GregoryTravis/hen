(load "h.ss")

(run-file "test-input.ss")

(map evl-check
     '(a
      ((/. r r) a)
      ((/. r s) a)
      ((/. r (s t)) a)
      ((/. p (p x)) (/. y y))
      ((/. p ((p a) b)) (/. s (/. t ((qq s) t))))
      ((+ 1) 2)
      ((= 3) 3)
      ((= 3) 4)
      (((if True) 1) 2)
      (((if False) 1) 2)
      ))
