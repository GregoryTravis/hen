(def glap 133)

(def fact (/. (n) (if (== n 0)
                      1
                      (* n (fact (- n 1))))))

(def poo (/. (a b) (+ a b)))

(def proo (/. (a b) (Goo b a)))

(def fact2
     (/./.
      (/. (0) 1)
      (/. (n) (* n (fact2 (- n 1))))))
(fun (fact3 0) 1)
(fun (fact3 n) (* n (fact3 (- n 1))))

(/. (x) 10)
((/. (x) 10) 1)
((/. (x) x) 1)
((/. (x) x) ((/. (x) x) 3))
((/. (x) glap) 1)
1
((/. (x) x) 3)
(+ glap 2)

(if 'True 1 2)
(if 'False 1 2)
(> 1 2)
(> 2 1)
(== 0 0)
(== 0 1)

(fact 10)
((/. () (poo ((/. () 1)) 2)))

(proo 1 2)
((/. (a b) (Hoo b a)) 1 2)
((/. (1 b) (Joo b b)) 1 2)
((/. (a 2) (Zoo a a)) 1 2)
((/. ('jerk b) (Roo b b)) 'jerk 2)

((/./. (/. (30) 3) (/. (a) 1)) 30)
((/./. (/. (30) 3) (/. (a) 1)) 20)

(fact2 10)
(fact3 10)

(fun (foo x) (* x 2))

(map foo (Cons 1 (Cons 2 Nil)))

(fun (hisk 1) True)
(fun (hisk x) False)

(grep hisk (Cons 1 (Cons 2 (Cons 3 (Cons 1 Nil)))))

(append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)))
(append Nil (Cons 3 (Cons 4 Nil)))
(append (Cons 3 (Cons 4 Nil)) Nil)

(reverse Nil)
(reverse (Cons 1 Nil))
(reverse (Cons 1 (Cons 2 Nil)))
(reverse (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))

(fun (foo a b) (Bar b b))

(foo 10 20)
(foo 20 10)

(Foo 10 20)
(Bar 10 20)
(Foo 20 10)
(Bar 20 10)

(fun (tak Nil) Nil)
(fun (tak (Cons a d)) (Cons (Harf a a) (tak d)))
(tak (Cons 1 (Cons 2 Nil)))

(fun (yep a) (Double a a))
(map yep (Cons 1 (Cons 2 Nil)))

(append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)))

(reverse (Cons 1 (Cons 2 Nil)))
(reverse (reverse (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))

(reverse (append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil))))

(+ 3 4)
(- 3 4)
(* 3 4)
(/ 3 4)

(== 3 4)
(!= 3 4)
(< 3 4)
(> 3 4)
(<= 3 4)
(>= 3 4)

(== 4 3)
(!= 4 3)
(< 4 3)
(> 4 3)
(<= 4 3)
(>= 4 3)

(== 4 4)
(!= 4 4)
(< 4 4)
(> 4 4)
(<= 4 4)
(>= 4 4)

(not False)
(not True)

(and True True)
(and False True)
(and True False)
(and False False)

(or True True)
(or False True)
(or True False)
(or False False)

(xor True False)
(xor False True)
(xor True True)
(xor False False)

(begin)
(begin (shew 1))
(begin (shew 1) (shew 2))
(begin (shew 1) (shew 2) (shew 3))

(shew)
(shew 1)
(shew 1 2)
(shew 1 2 3)
(shew 1 2 3 4)

(list)
(list 1)
(list 1 2)
(list 1 2 3)
(list 1 2 3 4)
