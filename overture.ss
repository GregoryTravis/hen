;; (fun (map f ()) ())
;; (fun (map f (cons a d)) (cons (f a) (map f d)))

;; (fun (grep p ()) [])
;; (fun (grep p (cons a d))
;;      (if (p a)
;;          (cons a (grep p d))
;;          (grep p d)))

;; (fun (append () a) a)
;; (fun (append (cons a d) b) (cons a (append d b)))

;; (fun (reverse ()) ())
;; (fun (reverse (cons a d))
;;      (append (reverse d) (cons a ())))

;; (fun (not 'true) false)
;; (fun (not 'false) true)

;; (fun (and 'true 'true) true)
;; (fun (and 'false 'true) false)
;; (fun (and 'true 'false) false)
;; (fun (and 'false 'false) false)

;; (fun (or 'true 'true) 'true)
;; (fun (or 'false 'true) 'true)
;; (fun (or 'true 'false) 'true)
;; (fun (or 'false 'false) 'false)

;; (fun (xor 'true 'false) 'true)
;; (fun (xor 'false 'true) 'true)
;; (fun (xor 'true 'true) 'false)
;; (fun (xor 'false 'false) 'false)

;; (fun (+ (integer a) (integer b))
;;      (integer (primitive-call (integer-+ a b))))
;; (fun (- (integer a) (integer b))
;;      (integer (primitive-call (integer-- a b))))
;; (fun (* (integer a) (integer b))
;;      (integer (primitive-call (integer-* a b))))
;; (fun (/ (integer a) (integer b))
;;      (integer (primitive-call (integer-/ a b))))

;; (fun (== (integer a) (integer b))
;;      (primitive-call (== a b)))

;; (fun (> (integer a) (integer b))
;;      (primitive-call (> a b)))

;; (fun (< a b)
;;      (not (or (> a b) (== a b))))

;; (fun (>= a b) (not (< a b)))
;; (fun (<= a b) (not (> a b)))

;; (fun (!= a b) (not (== a b)))

;; (fun (read-file (string filename))
;;      (primitive-call (read-file filename)))

;; (fun (begin a) a)
;; (fun (begin a b) b)
;; (fun (begin a b c) c)
;; (fun (begin a b c d) d)

;; (fun (shew a) (primitive-call (shew a)))
;; (fun (shew a b) (begin (shew a) (shew b)))
;; (fun (shew a b c) (begin (shew a) (shew b c)))
;; (fun (shew a b c d) (begin (shew a) (shew b c d)))
