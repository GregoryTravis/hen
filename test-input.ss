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

(fun (map f Nil) Nil)
(fun (map f (Cons a d)) (Cons (f a) (map f d)))

(fun (foo x) (* x 2))

(map foo (Cons 1 (Cons 2 Nil)))

(fun (grep p Nil) Nil)
(fun (grep p (Cons a d))
     (if (p a)
         (Cons a (grep p d))
         (grep p d)))

(fun (hisk 1) True)
(fun (hisk x) False)

(grep hisk (Cons 1 (Cons 2 (Cons 3 (Cons 1 Nil)))))

(fun (append Nil a) a)
(fun (append (Cons a d) b) (Cons a (append d b)))

(append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)))
(append Nil (Cons 3 (Cons 4 Nil)))
(append (Cons 3 (Cons 4 Nil)) Nil)

(fun (reverse Nil) Nil)
(fun (reverse (Cons a d))
     (append (reverse d) (Cons a Nil)))

(reverse Nil)
(reverse (Cons 1 Nil))
(reverse (Cons 1 (Cons 2 Nil)))
(reverse (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
