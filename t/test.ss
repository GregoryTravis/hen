(load "../lib.ss")

(shew 1)

(shew (mtch '(1) (a) 30)
      (mtch '(1 a) (b a) (+ b b))
      (mtch '(Jerk 1 2) (Jerk a b) (Fupp b a)))
