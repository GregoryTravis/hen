(fun (map 'f []) [])
(fun (map 'f ['a . 'd]) [('f 'a) . (map 'f 'd)])

(fun (swap (P 'a 'b)) (P 'b 'a))

(fun (allargs . 'a) (Jerk 'a))

(fun (all-true [True . 'rest]) (all-true 'rest))
(fun (all-true ['_ . 'rest]) (Fail))
(fun (all-true Nil) (ok))

(fun (main)
     (all-true
      [
       (== (var? Nil)  False)
       (== (data? Nil) True)
       (== (app? Nil) False)
       (== (var? 'a) True)
       (== (data? 'a) False)
       (== (app? 'a) False)
       (== (var? ['a]) False)
       (== (data? ['a]) False)
       (== (app? ['a]) True)
       (== (map swap (Cons (P A B) (Cons (P C D) Nil))) (Cons (P B A) (Cons (P D C) Nil)))
       (== (map swap [(P A B) (P C D)]) [(P B A) (P D C)])
       (== (if False Yes No) No)
       (== (if True Yes No) Yes)
       (== (if (== [a] [a]) Yes No) Yes)
       (== (if (== [a] [b]) Yes No) No)
       (== (allargs A B C) (Jerk (A B C)))
       ]))
