(fun (map 'f Nil) Nil)
(fun (map 'f (Cons 'a 'd)) (Cons ('f 'a) (map 'f 'd)))

(fun (swap (P 'a 'b)) (P 'b 'a))

(fun (main)
     (Voo
      (var? Nil)
      (data? Nil)
      (var? (Cons Nil Nil))
      (data? (Cons Nil Nil))
      (map swap (Cons (P A B) (Cons (P C D) Nil)))
      ))
