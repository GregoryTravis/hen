(fun (id x) x)

(fun (and True True) True)
(fun (and True False) False)
(fun (and False True) False)
(fun (and False False) False)
(fun (or True True) True)
(fun (or False True) True)
(fun (or True False) True)
(fun (or False False) False)
(fun (not True) False)
(fun (not False) True)
(fun (xor a b) (or (and a (not b)) (and (not a) b)))

(fun (if True a b) a)
(fun (if False a b) b)

(fun (append (Cons a as) bs) (Cons a (append as bs)))
(fun (append Nil bs) bs)

(fun (reverse Nil) Nil)
(fun (reverse (Cons a as)) (append (reverse as) (Cons a Nil)))
