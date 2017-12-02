(load "h.ss")

(set! make-show-commands #t)

;(display (mtch '(Foo 1) ('Foo a) a))
(build "src.ss")

;(skiff "ref")
(exit)
