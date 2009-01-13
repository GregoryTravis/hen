(fun (rbitsadd Nil Nil carry) (Cons carry Nil))
(fun (rbitsadd Nil b carry) (rbitsadd (Cons False Nil) b carry))
(fun (rbitsadd a Nil carry) (rbitsadd a (Cons False Nil) carry))
(fun (rbitsadd (Cons a as) (Cons b bs) carry)
     (vilk as bs (veet a b carry)))
     
(fun (vilk as bs (BitAndCarry bit carry)) (Cons bit (rbitsadd as bs carry)))
(fun (veet a b carry) (BitAndCarry (xor (xor a b) carry)
                                   (or (and (or a b) carry)
                                       (and a b))))

(fun (addy (Integer a) (Integer b))
     (Integer (strip-head-falses (reverse (rbitsadd (reverse a) (reverse b) False)))))

(fun (strip-head-falses (Cons False as)) (strip-head-falses as))
(fun (strip-head-falses x) x)

(addy 0 0)
(addy 0 1)
(addy 1 0)
(addy 1 1)
(addy 12 23)
;(addy 120 23)
