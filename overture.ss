(fun (map 'f []) [])
(fun (map 'f ['a . 'd]) [('f 'a) . (map 'f 'd)])

(fun (map 'f []) [])
(fun (map 'f ['a . 'd]) [('f 'a) . (map 'f 'd)])

(fun (swap (P 'a 'b)) (P 'b 'a))

(fun (allargs . 'a) (Jerk 'a))

(fun (all-true [True . 'rest]) (all-true 'rest))
(fun (all-true ['_ . 'rest]) (Fail))
(fun (all-true Nil) (ok))

