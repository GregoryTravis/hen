(load "../lib.ss")

(shew 1)

(shew (mtch '(1) (a) 30))
(shew (mtch '(1 a) (b a) (+ b b)))
(shew (mtch '(Jerk 1 2) ('Jerk a b) (list 'Fupp b a)))

(shew (mtch '(1) (a) a))

(shew (mtch '(1) (a) (cons a a) (b a) (+ b b)))
(shew (mtch '(1 2) (a) (cons a a) (b a) (+ b b)))
(shew (mtch '(1 2) (a) (cons a a) (a b) (+ b b)))
(shew (mtch '(Jerk 1 2) ('Jerk a b) (list 'Fupp b a)))
(shew (mtch '(q 1 2) ('q a b) b))
