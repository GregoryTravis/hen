;; (type (A b c) (B b b) (C b c))

;; (fun (foo (A (G (Int) x) y) p)
;;      (blah p))
;; =>
;; (fun (foo p)
;;      (guard (isType (A (G (Int) x) y) p))
;;      (blah p))

;(fun (isType (A (G g) (H m n))

;(isType (A (G (Int) 10) 20))

;; (fun (type-expand (A b c t))
;;      (list (B b b) (C b c)))


;; (type-expand (A (G g) (H m n) t))

(fun (isType (A b c) (B b0 b1))
     (and (isType b b0)
          (isType b b1)))

(fun (isType (A b c) (C b0 c0))
     (and (isType b b0)
          (isType c c0)))

;; (fun (foo (A (G g) (H m n) v))
;;      body)
;; =>
;; (fun (foo v)
;;      (guard (isType (A (G g) (H m n)) v))
;;      body)
;;
;; (foo (B (G 10) (G (20 30))))

;; Do I need one of these for every constructor?
;;
;; No, but you do for every alternative from a type def, perhaps.
(fun (isType (G g) (G g0)) 'true)
(fun (isType (H m n) (H m0 n0)) 'true)

(isType (A (G g) (H m n)) (B (G 10) (G (20 30))))
(isType (A (G g) (H m n)) (C (G 40) (H 50 60)))

