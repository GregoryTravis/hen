(load "../lib.ss")

(shew 1)

(map
 (lambda (p)
   (shew p)
   (let ((out (match-render-top (car p) (cadr p))))
     (shew out)
     (shew (eval out))))
 '(('(1) ((a) 30))
   ('(1 a) ((b a) (+ b b)))
   ('(Jerk 1 2) ((Jerk a b) (Fupp b a)))))
