(fun (map 'f []) [])
(fun (map 'f ['a . 'd]) [('f 'a) . (map 'f 'd)])
(fun (swap (P 'a 'b)) (P 'b 'a))

(fun (main) (map swap [(P A B) (P C D)]))
