((/. (x) x) Nil)

((/. ((PP j k)) j) (PP 10 20))
((/. ((PP j k)) k) (PP 10 20))
((/. ((PP (PP j jj) (PP k r))) j) (PP (PP 10 100) (PP 20 30)))
((/. ((PP (PP j jj) (PP k r))) jj) (PP (PP 10 100) (PP 20 30)))
((/. ((PP (PP j jj) (PP k r))) k) (PP (PP 10 100) (PP 20 30)))
((/. ((PP (PP j jj) (PP k r))) r) (PP (PP 10 100) (PP 20 30)))

((/. ((PP j (PP k r))) j) (PP 10 (PP 20 30)))
((/. ((PP j (PP k r))) k) (PP 10 (PP 20 30)))
((/. ((PP j (PP k r))) r) (PP 10 (PP 20 30)))

((/. ((PP (PP j jj) q)) j) (PP (PP 10 100) 20))
((/. ((PP (PP j jj) q)) jj) (PP (PP 10 100) 20))
((/. ((PP (PP j jj) q)) q) (PP (PP 10 100) 20))

((/. (x) x) 1)
; varargs-binding
;((/. (x) (x 2)) (/. x x))
((/. (x) (x 2)) (/. (x) x))
;(/. (r) r)
((/. (r) r) 3)
;((+ 10) 20)
;((+ ((/. (x) x) 2000)) 3000)
((== 30) 30)
((== 30) 40)
(((if True) 100) 200)
(((if False) 100) 200)

(def tuct (/. (x) x))

(tuct 10)
;((+ 20) (tuct 10))
;((- 40) 15)
;((* 10) 20)

(((if ((== 0) 3)) True) False)
(((if ((== 0) 0)) True) False)
((/. (n) (((if ((== n) 3)) True) False)) 3)
((/. (n) (((if ((== n) 3)) True) False)) 0)
;(((if ((== 0) 0)) ((* 10) 20)) ((* 30) 40))
;(((if ((== 3) 0)) ((* 10) 20)) ((* 30) 40))
;((/. (n) (((if ((== n) 0)) 1) ((* n) ((- n) 1)))) 3)

(cons 1 2)
(car (cons 1 2))
(cdr (cons 1 2))
(pair? (cons 1 2))
(pair? (car (cons 1 2)))
(pair? (cdr (cons 1 2)))
(pair? (Foo 10))
(pair? 10)
(pair? (car (cons (cons 1 2) 3)))

;(def double (/. (x) ((+ x) x)))
;(double 10)

(def ones
  (/./.
   (/. ((PP aaa d)) (cons (PP 1 aaa) (ones d)))
   (/. (x) x)))
(ones Nil)
(ones (PP 1 Nil))
(ones (PP 1 (PP 2 (PP 3 Nil))))

(def map3
     (/. (f)
         (/./.
          (/. ((PP a d)) (PP (f a) ((map3 f) d)))
          (/. (x) x))))

;((map3 double) (PP 1 (PP 2 (PP 3 Nil))))
;((map3 double) 1)
 
((/. (a . b) a) 1 . 2)
((/. (a . b) b) 1 . 2)

(def voo
     (/./.
;      (/. (PP a (PP b c)) ((+ b) c))
      (/. (a b . c) (cons b c))
;      (/. (PP a b) b)
      (/. (a . b) b)))

;(voo (PP 1 (PP 2 3)))
(voo 1 2 . 3)

; varargs-binding
;; (/. x x)
;; ((/. x x) 1)
;; ((/. (a . b) 1) 10 . 20)
;; ((/. (a . b) a) 10 . 20)
;; ((/. (a . b) b) 10 . 20)
;;; ((/. (a . b) ((+ a) b)) 10 . 20)

;; - will (/. (Foo a) ..) ever mean anything?
;; (def voo2
;;      (/./.
;;      (/. (PP f (PP a b)) (cons 20 (voo2 (PP f b))))
;;       (/. (f a . b) (cons 20 (voo2 (PP f b))))
;;      (/. (PP f x) x)
;;       (/. (f . x) x)))
;; (voo2 double 1 . Nil)
;; (voo2 double . Nil)

; varargs-binding
;; ((/. (f a . b) b) 1000 1 . 2)
;; ((/. (a . b) a) 1 . 2)

(def map2
     (/./.
      (/. (f (PP a b)) (PP (f a) (map2 f b)))
      (/. (f Nil) Nil)))

;(map2 double (PP 1 (PP 2 (PP 3 Nil))))

((/. 1 2) . 1)
((/. (1) 2) 1)

((/./. (/. 1 100) (/. 2 200)) . 1)
((/./. (/. 1 100) (/. 2 200)) . 2)
((/./. (/. (1) 100) (/. (2) 200)) 1)
((/./. (/. (1) 100) (/. (2) 200)) 2)

;((/. (Nil) Nil) Nil)
((/. 'joe 1) . 'joe)
((/. ('joe) 1) 'joe)
((/. (joe) joe) 1)
; Shouldn't work, but does, because (@ joee) is never evaluated.
((/. (joe) 1) joee)

((/. (1 2) 40) 1 2)
;((/. (a b) ((+ a) b)) 1 2)

1
; varargs-binding
;; (/. x x)
;; ((/. x x) 1)
;; ((/. y ((/. x x) y)) 2)
;; (/. (a . b) a)
;; ((/. (a . b) a) 1 . 2)
;; ((/. (a . b) b) 1 . 2)

(Foo 1 2)
((/. ((Foo a b)) a) (Foo 10 20))
((/. ((Foo a b)) b) (Foo 10 20))
;((/. ((Foo a b)) ((+ a) b)) (Foo 10 20))
((/. ((Foo (Bar a) b)) a) (Foo (Bar 10) 20))
((/. ((Foo (Bar a) b)) b) (Foo (Bar 10) 20))
;((/. ((Foo (Bar a) b)) ((+ a) b)) (Foo (Bar 10) 20))
((/. ((Foo a b)) a) (Foo (Bar 10) 20))
((/. ((Foo a b)) b) (Foo (Bar 10) 20))

(fun (ones2 (PP aaa d)) (cons (PP 1 aaa) (ones2 d)))
(fun (ones2 x) x)
(ones2 Nil)
(ones2 (PP 1 Nil))
(ones2 (PP 1 (PP 2 (PP 3 Nil))))

(fun (voo20 a b . c) (cons b c))
(fun (voo20 a . b) b)

(voo20 1 2 . 3)
(voo20 1 . 10)

; varargs-binding
;; (fun (voo22 f a . b) (cons 20 (voo22 (PP f b))))
;; (fun (voo22 f . x) x)
;; (voo22 double 1 . Nil)
;; (voo22 double . Nil)

(fun (map22 f (PP a b)) (PP (f a) (map22 f b)))
(fun (map22 f Nil) Nil)

;(map22 double (PP 1 (PP 2 (PP 3 Nil))))

(fun (boit 1) 100)
(fun (boit 2) 200)
(boit 1)
(boit 2)

(fun (boit2 . 1) 100)
(fun (boit2 . 2) 200)
(boit2 . 1)
(boit2 . 2)

(Cons 1 (Cons 2 Nil))
($ 1 2)
(car ($ 1 2))
(cdr ($ 1 2))
(car (cdr ($ 1 2)))
(cdr (cdr ($ 1 2)))

(fun (fact 0) 1)
;(fun (fact n) ((* n) (fact ((- n) 1))))
;(fact 10)
