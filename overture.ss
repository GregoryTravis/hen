(fun (map f Nil) Nil)
(fun (map f (Cons a d)) (Cons (f a) (map f d)))

(fun (grep p Nil) Nil)
(fun (grep p (Cons a d))
     (if (p a)
         (Cons a (grep p d))
         (grep p d)))

(fun (append Nil a) a)
(fun (append (Cons a d) b) (Cons a (append d b)))

(fun (reverse Nil) Nil)
(fun (reverse (Cons a d))
     (append (reverse d) (Cons a Nil)))

(fun (not True) False)
(fun (not False) True)

(fun (and True True) True)
(fun (and False True) False)
(fun (and True False) False)
(fun (and False False) False)

(fun (or True True) True)
(fun (or False True) True)
(fun (or True False) True)
(fun (or False False) False)

(fun (xor True False) True)
(fun (xor False True) True)
(fun (xor True True) False)
(fun (xor False False) False)

(fun (< a b)
     (not (or (> a b) (== a b))))

(fun (>= a b) (not (< a b)))
(fun (<= a b) (not (> a b)))

(fun (!= a b) (not (== a b)))

;; (fun (read-file (String filename))
;;      (primitive-call (read-file filename)))

(fun (begin) Mu)
(fun (begin a) a)
(fun (begin a . rest) (begin . rest))

(fun (shew) Mu)
(fun (shew a . d)
     (begin
       (__prim-shew a)
       (shew . d)))

(fun (list . args) args)
(fun (cons a d) (list a . d))

(fun (interactive-shew Mu) Mu)
(fun (interactive-shew a) (shew a))
