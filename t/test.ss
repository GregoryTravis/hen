(load "../lib.ss")

(shew 1)

(shew (mtch '(1) (a) 30))
(shew (mtch '(1 a) (b a) (+ b b)))
(shew (mtch '(Jerk 1 2) ('Jerk a b) (list 'Fupp b a)))
