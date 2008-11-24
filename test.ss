(load "h.ss")

(run-file "test-input.ss")

(map evl-check
     '(;a
       ((/. r r) 1)
       ((/. p (p 2)) (/. y y))
       ((+ 1) 2)
;;        ((= 3) 3)
;;        ((= 3) 4)
;       (((if True) 1) 2)
;       (((if False) 1) 2)
       ))
