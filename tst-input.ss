(fun (foo a b) (bar b b))
(foo 10 20)
(bar 10 20)
(foo 20 10)
(bar 20 10)

(fun (tak []) [])
(fun (tak (cons a d)) (cons (harf a a) (tak d)))
(tak [1 2])

(fun (yep a) (double a a))
(map yep [1 2])

(append [1 2] [3 4])

(reverse [1 2])
(reverse [1 2 3 4])

(reverse (append [1 2] [3 4]))

(+ 3 4)
(- 3 4)
(* 3 4)
(/ 3 4)

(= 3 4)
(!= 3 4)
(< 3 4)
(> 3 4)
(<= 3 4)
(>= 3 4)

(= 4 3)
(!= 4 3)
(< 4 3)
(> 4 3)
(<= 4 3)
(>= 4 3)

(= 4 4)
(!= 4 4)
(< 4 4)
(> 4 4)
(<= 4 4)
(>= 4 4)

(not 'false)
(not 'true)

(and 'true 'true)
(and 'false 'true)
(and 'true 'false)
(and 'false 'false)

(or 'true 'true)
(or 'false 'true)
(or 'true 'false)
(or 'false 'false)

(xor 'true 'false)
(xor 'false 'true)
(xor 'true 'true)
(xor 'false 'false)

(not false)
(not true)

(and true true)
(and false true)
(and true false)
(and false false)

(or true true)
(or false true)
(or true false)
(or false false)

(xor true false)
(xor false true)
(xor true true)
(xor false false)

(fun (fact n)
     (if (= n 0)
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
(map 'shew [5 5 6])
(map shew (form->list (5 5 6)))
(shew [4 5])

(fun (blah x) (> x 10))
(fun (bluh x) (< x 10))

(grep blah [1 2 30 40 5])
(grep bluh [1 2 30 40 5])
