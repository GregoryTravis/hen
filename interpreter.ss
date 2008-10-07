;; (fun (run src)
;;      (shew src))

;; (run (read-file "runme.ss"))

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

;; (match-descend (Var a) 10)
;; (match-descend (Pair (Var a) (Var b)) (Pair 30 40))
;; (match-descend (Pair (Var a) (Pair (Var b) (Var c))) (Pair 30 40))
;; (match-descend (Constant 10) (Constant 10))
;; (match-descend (Constant 10) (Constant 20))
;; (match-descend (Constant joe) (Constant joe))
;; (match-descend (Constant joe) (Constant blah))

;; (match-descend (Pair (Constant Food) (Pair (Var a) Nil))
;;                (Pair (Constant Food) (Pair 10 Nil)))

(fun (uuf [a . d]) (Cons (uuf a) (uuf d)))
(fun (uuf []) [])
(fun (uuf (Integer (Primitive a))) (Constant (Integer (Primitive 1))))
(fun (uuf a) a)

;(uuf 1)
;(match-descend (uuf 1) (uuf 1))

1
jerk
