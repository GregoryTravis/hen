;(fun (bug 1 b) 10)
;(fun (bug a 2) 20)
;(fun (bug a a) 30)
;; (fun (bug a b) 40)

;; (bug 1 6)
;; (bug 7 2)
;; (bug 12 12)
;; (bug 13 15)

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
