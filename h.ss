(load "lib.ss")

(define forms (read-objects "src.ss"))

(define evl shew)

(map evl forms)
