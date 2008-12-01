((/. x x) 1)
((/. x (x 2)) (/. x x))
;a
(/. r r)
((/. r r) 3)
;(/. r s)
;((/. r s) a)
;(/. r (s t))
;((/. r (s t)) 4)
;(/. r (/. s s))
;(/. r (/. s r))
;((/. p (p x)) (/. y y))
;((/. p ((p a) b)) (/. s (/. t ((qq s) t))))
((+ 10) 20)
((+ ((/. x x) 2000)) 3000)
((== 30) 30)
((== 30) 40)
(((if True) 100) 200)
(((if False) 100) 200)
(fun (tuct x) x)
(tuct 10)
((+ 20) (tuct 10))
((- 40) 15)
((* 10) 20)

(fun (fact1 n) (((if ((== n) 0)) 1) ((* n) (fact1 ((- n) 1)))))
(fact1 10)

(((if ((== 0) 3)) True) False)
(((if ((== 0) 0)) True) False)
((/. n (((if ((== n) 3)) True) False)) 3)
((/. n (((if ((== n) 3)) True) False)) 0)
(((if ((== 0) 0)) ((* 10) 20)) ((* 30) 40))
(((if ((== 3) 0)) ((* 10) 20)) ((* 30) 40))
((/. n (((if ((== n) 0)) 1) ((* n) ((- n) 1)))) 3)

((cons 1) 2)
(car ((cons 1) 2))
(cdr ((cons 1) 2))
(pair? ((cons 1) 2))
(pair? (car ((cons 1) 2)))
(pair? (cdr ((cons 1) 2)))

(def ones
  (/./.
   (/. ((P a) d) ((cons a) (ones d)))
   (/. x x)))
(ones ((P 1) ((P 2) ((P 3) Nil))))

(def double (/. x ((+ x) x)))
(double 10)

((map double) ((P 1) ((P 2) ((P 3) Nil))))

((/. ((P a) b) a) ((P 1) 2))

(def voo
     (/./.
      (/. ((P a) ((P b) c)) ((+ b) c))
      (/. ((P a) b) b)))

(voo ((P 1) ((P 2) 3)))

(/. x x)
((/. x x) 1)
((/. ((P a) b) 1) ((P 10) 20))
((/. ((P a) b) a) ((P 10) 20))
((/. ((P a) b) b) ((P 10) 20))
((/. ((P a) b) ((+ a) b)) ((P 10) 20))

(def voo2
     (/./.
      (/. ((P f) ((P a) b)) ((cons 20) (voo2 ((P f) b))))
      (/. ((P f) x) x)))
(def double (/. x ((+ x) x)))

(voo2 ((P double) ((P 1) Nil)))
(voo2 ((P double) Nil))

((/. ((P f) ((P a) b)) b) ((P 1000) ((P 1) 2)))
((/. ((P a) b) a) ((P 1) 2))
