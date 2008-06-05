(fun (f (bark a) (jok b t y)) (lemon a y))
(fun (loo a) (f (bark a) (jok a a (+ a a))))

(f (bark 20) (jok (var 10) 20 (gurk 30 40)))

(loo 44)

(fun (la (cons a d)) (cons a (joe (la d))))
(fun (la 'nil) 'nil)
(la (cons 1 (cons 2 (cons 3 'nil))))

(asdf 'asd)

(+ 1 (+ 2 20))

(fun (hi1 (r 'f)) 67)
(fun (hi2 (r f)) 67)

(hi1 (r 'f))
(hi2 (r 'f))

(= 1 2)
(= 1 1)

(fun (bif 'true t e) t)
(fun (bif 'false t e) e)

(bif (= 1 2) 'right 'wrong)
(bif (= 1 1) 'right 'wrong)

(fun (a-fun (+ a b)) 100)
(fun (a-fun c) 200)

(macro (a-macro (+ a b)) 100)
(macro (a-macro c) 200)

(a-fun (+ 1 2))
(a-macro (+ 1 2))

;;;;;;;;;;;

;; (type (A b c) (B b b) (C b c))

;; (fun (foo (A (G g) (H m n) v))
;;      body)
;; =>
;; (fun (foo v)
;;      (guard (isType (A (G g) (H m n)) v))
;;      body)
;;
;; (foo (B (G 10) (G (20 30))))

(fun (isType (A b c) (B b0 b1))
     (and (isType b b0)
          (isType b b1)))

(fun (isType (A b c) (C b0 c0))
     (and (isType b b0)
          (isType c c0)))

;; Do I need one of these for every constructor?
;;
;; No, but you do for every alternative from a type def, perhaps.
;;
;; No, actually, you need it for every possible constructor.
(fun (isType (G g) (G g0)) 'true)
(fun (isType (H m n) (H m0 n0)) 'true)

;; With this, you don't need them for every constructor because the
;; only time isType will ever be called is on an alternative of a
;; type.
;;
;; But wait, where exactly would this be defined?  It needs to come
;; after everything else.  Problem.
(fun (isType a b) 'false)

(isType (A (G 'g) (H 'm 'n)) (B (G 10) (G (20 30))))
(isType (A (G 'g) (H 'm 'n)) (C (G 40) (H 50 60)))

(isType (A (G 'g) (H 'm 'n)) (B (G 10) (H 50 60)))
(isType (A (G 'g) (H 'm 'n)) (C (G 10) (G (20 30))))

(isType (A (G 'g) (H 'm 'n)) (Q (G 10) (H 50 60)))
(isType (A (G 'g) (H 'm 'n)) (R (G 10) (G (20 30))))

(fun (bar a) (list a a))
(bar 10)

(fun (foo a) (? 'true) (list a a))
(foo 20)

(fun (baz a) (? 'false) (list a a))
(baz 30)
