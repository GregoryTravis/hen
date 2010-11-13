(fun (map 'f []) [])
(fun (map 'f ['a . 'd]) [('f 'a) . (map 'f 'd)])
(fun (swap (P 'a 'b)) (P 'b 'a))
