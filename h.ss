(load "lib.ss")

(define forms (read-objects "src.ss"))

(define eval shew)

(map eval forms)
