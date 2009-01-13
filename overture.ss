;; id
(fun (id x) x)

;; boolean algebra
(fun (and True True) True)
(fun (and True False) False)
(fun (and False True) False)
(fun (and False False) False)
(fun (or a b) (not (and (not a) (not b))))
(fun (not True) False)
(fun (not False) True)
(fun (xor a b) (or (and a (not b)) (and (not a) b)))

;; if
(fun (if True a b) a)
(fun (if False a b) b)

;; list things
(fun (append (Cons a as) bs) (Cons a (append as bs)))
(fun (append Nil bs) bs)

(fun (reverse Nil) Nil)
(fun (reverse (Cons a as)) (append (reverse as) (Cons a Nil)))

(fun (map f (Cons a as)) (Cons (f a) (map f as)))
(fun (map f Nil) Nil)

;; Addition
(fun (rbitsadd Nil Nil carry) (Cons carry Nil))
(fun (rbitsadd Nil b carry) (rbitsadd (Cons False Nil) b carry))
(fun (rbitsadd a Nil carry) (rbitsadd a (Cons False Nil) carry))
(fun (rbitsadd (Cons a as) (Cons b bs) carry)
     (vilk as bs (veet a b carry)))
     
(fun (vilk as bs (BitAndCarry bit carry)) (Cons bit (rbitsadd as bs carry)))
(fun (veet a b carry) (BitAndCarry (xor (xor a b) carry)
                                   (or (and (or a b) carry)
                                       (and a b))))

(fun (+ (Integer a) (Integer b))
     (Integer (strip-head-falses (reverse (rbitsadd (reverse a) (reverse b) False)))))

(fun (strip-head-falses (Cons False as)) (strip-head-falses as))
(fun (strip-head-falses x) x)
