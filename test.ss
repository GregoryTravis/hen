(load "h.ss")

;(run-file "test-input.ss")

(map
 (lambda (x)
   (shew x)
   (shew (ski x))
   (shew (evl (ski x))))
 (list
  'a
  '(/. r r)
  '((/. r r) a)
  '(/. r s)
  '((/. r s) a)
  '(/. r (s t))
  '((/. r (s t)) a)
  '(/. r (/. s s))
  '(/. r (/. s r))
  '((/. p (p x)) (/. y y))
  '((/. p ((p a) b)) (/. s (/. t ((qq s) t))))
))
