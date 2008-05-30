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

(isType (A (G g) (H m n)) (B (G 10) (G (20 30))))
(isType (A (G g) (H m n)) (C (G 40) (H 50 60)))

(isType (A (G g) (H m n)) (B (G 10) (H 50 60)))
(isType (A (G g) (H m n)) (C (G 10) (G (20 30))))

(isType (A (G g) (H m n)) (Q (G 10) (H 50 60)))
(isType (A (G g) (H m n)) (R (G 10) (G (20 30))))
