(fun (map 'f Nil) Nil)
(fun (map 'f (Cons 'a 'd)) (Cons ('f 'a) (map 'f 'd)))

(fun (swap (P 'a 'b)) (P 'b 'a))

(fun (main)
     (Voo
      (var? Nil)
      (data? Nil)
      (app? Nil)
      (var? 'a)
      (data? 'a)
      (app? 'a)
      (var? (Cons 'a Nil))
      (data? (Cons 'a Nil))
      (app? (Cons 'a Nil))
      (map swap (Cons (P A B) (Cons (P C D) app)))
      ))
