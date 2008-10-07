(fun (foo a b) (Bar b b))

(foo 10 20)
(foo 20 10)

(Foo 10 20)
(Bar 10 20)
(Foo 20 10)
(Bar 20 10)

(fun (tak []) [])
(fun (tak (Cons a d)) (Cons (Harf a a) (tak d)))
(tak [1 2])

(fun (yep a) (Double a a))
(map yep [1 2])

(append [1 2] [3 4])

(reverse [1 2])
(reverse [1 2 3 4])

(reverse (append [1 2] [3 4]))

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

(fun (fact n)
     (if (== n 0)
         1
         (* n (fact (- n 1)))))
(fact 10)

(fun (fact2 0) 1)
(fun (fact2 n)
     (* n (fact2 (- n 1))))
(fact2 10)

"a"

(read-file "tst-readee.ss")

(shew 5 5 6)
(map shew [5 5 6])
(shew [4 5])

(fun (blah x) (> x 10))
(fun (bluh x) (< x 10))

(grep blah [1 2 30 40 5])
(grep bluh [1 2 30 40 5])

(begin 5)
(begin 5 6)
(begin 5 6 7)
(begin 5 6 7 8)

(shew 5)
(shew 5 6)
(shew 5 6 7)
(shew 5 6 7 8)

(var a-global 10)
(shew a-global)
(+ a-global a-global)
a-global

(fun (guardo-a a b)
     (? True)
     (Bar a a))

(fun (guardo-b a b)
     (? False)
     (Bar a a))

(fun (guardo-b a b)
     (Voo b a))

(fun (guardo-c a b)
     (? (== a b))
     (Bar a a))

(fun (guardo-c a b)
     (Bic b b))

(guardo-a 10 10)
(guardo-b 10 50)
(guardo-c 10 10)
(guardo-c 10 20)

(fun (jerk a b c)
     (? (== a (Cons b c)))
     10)
(fun (jerk a b c)
     20)

(jerk (Cons 1 2) 1 2)
(jerk (Cons 1 2) 2 1)

(fun (jerku a a b c c) 12)
(fun (jerku q a b c c) 0)
(fun (jerku a a b c d) 4)
(fun (jerku a v b c d) 18)

(jerku 1 1 2 3 3)
(jerku 10 1 2 3 3)
(jerku 1 1 2 3 30)
(jerku 10 1 2 3 30)

(jerku (Poo 34) (Poo 34) (G) (Lap 5) (Lap 6))
(jerku (Poo 34) (Poo 34) (G) (Lap 5) (Lap 5))

(fun (figg a b)
     (? (== a (Jerk)))
     (Shoe a b))
(fun (figg a b) (Blap a b))

(figg 10 20)
(figg (Jerk) 20)

(fun (asldfih a b) a b)
(fun (asdfuae a b) (? True) a b)
(asldfih 10 20)
(asdfuae 10 20)

;; interpreter...

(fun (maybe-append (Just as) (Just bs))
     (Just (append as bs)))
(fun (maybe-append a (Fail)) (Fail))
(fun (maybe-append (Fail) b) (Fail))

(fun (match-descend (Var name) b) (Just [(Binding (Var name) b)]))
(fun (match-descend (Pair a b) (Pair c d))
     (maybe-append (match-descend a c)
                   (match-descend b d)))
(fun (match-descend (Pair a b) blah) (Fail))
(fun (match-descend (Constant c) (Constant c)) (Just []))
(fun (match-descend (Constant c) (Constant d)) (Fail))
(fun (match-descend Nil Nil) (Just []))

(match-descend (Var a) 10)
(match-descend (Pair (Var a) (Var b)) (Pair 30 40))
(match-descend (Pair (Var a) (Pair (Var b) (Var c))) (Pair 30 40))
(match-descend (Constant 10) (Constant 10))
(match-descend (Constant 10) (Constant 20))
(match-descend (Constant joe) (Constant joe))
(match-descend (Constant joe) (Constant blah))

(match-descend (Pair (Constant Food) (Pair (Var a) Nil))
               (Pair (Constant Food) (Pair 10 Nil)))

(fun (uuf [a . d]) (Cons (uuf a) (uuf d)))
(fun (uuf []) [])
(fun (uuf (Integer (Primitive a))) (Constant (Integer (Primitive 1))))
(fun (uuf a) a)

(uuf 1)
(match-descend (uuf 1) (uuf 1))
