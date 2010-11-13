(fun (map 'f Nil) Nil)
(fun (map 'f (Cons 'a 'd)) (Cons ('f 'a) (map 'f 'd)))

(fun (swap (P 'a 'b)) (P 'b 'a))

(fun (allargs . 'a) (Jerk 'a))

(fun (all-true True . 'rest) (all-true . 'rest))
(fun (all-true) (ok))
(fun (all-true _) (Fail))

(fun (main)
     (all-true
      (== (var? Nil)  False)
      (== (data? Nil) True)
      (== (app? Nil) False)
      (== (var? 'a) True)
      (== (data? 'a) False)
      (== (app? 'a) False)
      (== (var? (Cons 'a Nil)) False)
      (== (data? (Cons 'a Nil)) False)
      (== (app? (Cons 'a Nil)) True)
      (== (map swap (Cons (P A B) (Cons (P C D) app))) (Cons (P B A) (Cons (P D C) (map swap app))))
      (== (if False Yes No) No)
      (== (if True Yes No) Yes)
      (== (if (== (Cons a Nil) (Cons a Nil)) Yes No) Yes)
      (== (if (== (Cons a Nil) (Cons b Nil)) Yes No) No)
      (== (allargs A B C) (Jerk [A B C]))
      ))
