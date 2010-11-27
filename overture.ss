(fun (map 'f []) [])
(fun (map 'f ['a . 'd]) [('f 'a) . (map 'f 'd)])

(fun (map 'f []) [])
(fun (map 'f ['a . 'd]) [('f 'a) . (map 'f 'd)])

(fun (swap (P 'a 'b)) (P 'b 'a))

(fun (allargs . 'a) (Jerk 'a))

(fun (all-true [True . 'rest]) (all-true 'rest))
(fun (all-true ['_ . 'rest]) (Fail))
(fun (all-true Nil) (ok))

(fun (foldl 'f 'z (Cons 'x 'xs)) (foldl 'f ('f 'z 'x) 'xs))
(fun (foldl 'f 'z Nil) 'z)

(fun (foldr 'f 'z Nil) 'z)
(fun (foldr 'f 'z (Cons 'x 'xs)) ('f 'x (foldr 'f 'z 'xs)))

(fun (cons 'x 'xs) (Cons 'x 'xs))
