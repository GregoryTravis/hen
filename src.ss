;; ;; (read-file "tst-readee.ss")

;; (begin 5)
;; (begin 5 6)
;; (begin 5 6 7)
;; (begin 5 6 7 8)

;; (shew 5)
;; (shew 5 6)
;; (shew 5 6 7)
;; (shew 5 6 7 8)

(map 'shew [5 5 6])
(shew [4 5])

;; (fun (blah x) (> x 10))
;; (fun (bluh x) (< x 10))

;; (grep 'blah [1 2 30 40 5])
;; (grep 'bluh [1 2 30 40 5])
;; (grep blah [1 2 30 40 5])
;; (grep bluh [1 2 30 40 5])
