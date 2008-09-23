(fun (map f ()) ())
(fun (map f (Cons a d)) (Cons (f a) (map f d)))

(fun (grep p ()) [])
(fun (grep p (Cons a d))
     (if (p a)
         (Cons a (grep p d))
         (grep p d)))

(fun (append () a) a)
(fun (append (Cons a d) b) (Cons a (append d b)))

(fun (reverse ()) ())
(fun (reverse (Cons a d))
     (append (reverse d) (Cons a ())))

(fun (not 'true) false)
(fun (not 'false) true)

(fun (and 'true 'true) true)
(fun (and 'false 'true) false)
(fun (and 'true 'false) false)
(fun (and 'false 'false) false)

(fun (or 'true 'true) true)
(fun (or 'false 'true) true)
(fun (or 'true 'false) true)
(fun (or 'false 'false) false)

(fun (xor 'true 'false) true)
(fun (xor 'false 'true) true)
(fun (xor 'true 'true) false)
(fun (xor 'false 'false) false)

(fun (+ (Integer a) (Integer b))
     (Integer (primitive-call (integer-+ a b))))
(fun (- (Integer a) (Integer b))
     (Integer (primitive-call (integer-- a b))))
(fun (* (Integer a) (Integer b))
     (Integer (primitive-call (integer-* a b))))
(fun (/ (Integer a) (Integer b))
     (Integer (primitive-call (integer-/ a b))))

(fun (== a b) (primitive-call (== a b)))

(fun (> (Integer a) (Integer b))
     (primitive-call (> a b)))

(fun (< a b)
     (not (or (> a b) (== a b))))

(fun (>= a b) (not (< a b)))
(fun (<= a b) (not (> a b)))

(fun (!= a b) (not (== a b)))

(fun (read-file (String filename))
     (primitive-call (read-file filename)))

(fun (begin a) a)
(fun (begin a b) b)
(fun (begin a b c) c)
(fun (begin a b c d) d)

(fun (shew a) (primitive-call (shew a)))
(fun (shew a b) (begin (shew a) (shew b)))
(fun (shew a b c) (begin (shew a) (shew b c)))
(fun (shew a b c d) (begin (shew a) (shew b c d)))
