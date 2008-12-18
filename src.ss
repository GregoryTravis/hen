((/. (x) x) Nil)

((/. ((P j k)) j) (P 10 20))
((/. ((P j k)) k) (P 10 20))
((/. ((P (P j jj) (P k r))) j) (P (P 10 100) (P 20 30)))
((/. ((P (P j jj) (P k r))) jj) (P (P 10 100) (P 20 30)))
((/. ((P (P j jj) (P k r))) k) (P (P 10 100) (P 20 30)))
((/. ((P (P j jj) (P k r))) r) (P (P 10 100) (P 20 30)))

((/. ((P j (P k r))) j) (P 10 (P 20 30)))
((/. ((P j (P k r))) k) (P 10 (P 20 30)))
((/. ((P j (P k r))) r) (P 10 (P 20 30)))

((/. ((P (P j jj) q)) j) (P (P 10 100) 20))
((/. ((P (P j jj) q)) jj) (P (P 10 100) 20))
((/. ((P (P j jj) q)) q) (P (P 10 100) 20))

((/. (x) x) 1)
((/. (x) (x 2)) (/. x x))
((/. (x) (x 2)) (/. (x) x))
(/. (r) r)
((/. (r) r) 3)
((+ 10) 20)
((+ ((/. (x) x) 2000)) 3000)
((== 30) 30)
((== 30) 40)
(((if True) 100) 200)
(((if False) 100) 200)

(def tuct (/. (x) x))

(tuct 10)
((+ 20) (tuct 10))
((- 40) 15)
((* 10) 20)

;; (fun (fact1 n) (((if ((== n) 0)) 1) ((* n) (fact1 ((- n) 1)))))
;; (fact1 10)

(((if ((== 0) 3)) True) False)
(((if ((== 0) 0)) True) False)
((/. (n) (((if ((== n) 3)) True) False)) 3)
((/. (n) (((if ((== n) 3)) True) False)) 0)
(((if ((== 0) 0)) ((* 10) 20)) ((* 30) 40))
(((if ((== 3) 0)) ((* 10) 20)) ((* 30) 40))
((/. (n) (((if ((== n) 0)) 1) ((* n) ((- n) 1)))) 3)

((cons 1) 2)
(car ((cons 1) 2))
(cdr ((cons 1) 2))
(pair? ((cons 1) 2))
(pair? (car ((cons 1) 2)))
(pair? (cdr ((cons 1) 2)))

(def double (/. (x) ((+ x) x)))
(double 10)

(def ones
  (/./.
   (/. ((P aaa d)) ((cons (P 1 aaa)) (ones d)))
   (/. (x) x)))
(ones Nil)
(ones (P 1 Nil))
(ones (P 1 (P 2 (P 3 Nil))))
