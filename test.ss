(load "h.ss")

(run-file "test-input.ss")

(shew
 (let ((a (simplify-/. '(/. (P a b) ((+ a) b)) 'FAIL)))
   (list
    a
    (ski a)
    (evl (ski `(,a ((cons 1) 2)))))))

(shew 
 (let ((a
        (simplify-/./.
         '((/. (P a b) ((+ a) b))
           (/. x 100)))))
   (list a (ski a) (evl (ski `(,a ((cons 1) 2)))) (evl (ski `(,a 40))))))

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
