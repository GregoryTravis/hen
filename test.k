(fun (map 'f Nil) Nil)
(fun (map 'f (Cons 'a 'd)) (Cons ('f 'a) (map 'f 'd)))

(fun (swap (P 'a 'b)) (P 'b 'a))

(fun (main)
     (Tests
      (= (var? Nil)  False)
      (= (data? Nil) True)
      (= (app? Nil) False)
      (= (var? 'a) True)
      (= (data? 'a) False)
      (= (app? 'a) False)
      (= (var? (Cons 'a Nil)) False)
      (= (data? (Cons 'a Nil)) False)
      (= (app? (Cons 'a Nil)) True)
      (= (map swap (Cons (P A B) (Cons (P C D) app))) (Cons (P B A) (Cons (P D C) (map swap app))))
      ))
