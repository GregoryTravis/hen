(load "lib.ss")

(display (mtch '(1 2)
               (a b) (list b a)
               (a b c) (list b a c)
               ))
