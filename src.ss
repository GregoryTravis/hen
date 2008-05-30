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

(fun (isType (G g) (G g0)) 'true)

(isType (A (G g) (H m n)) (B (G 10) (G (20 30))))
