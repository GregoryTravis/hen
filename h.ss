(load "lib.ss")

(define forms (read-objects "src.ss"))
(define (go) (map evl forms))

(define evl shew)
