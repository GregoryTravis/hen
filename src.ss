;; ;(fun (add a b) (+ a b))

;; (fun (a-fun (+ a b)) 100)
;; (fun (a-fun c) 200)

;; (macro (a-macro (+ a b)) 100)
;; (macro (a-macro c) 200)

;; (a-fun (+ 1 2))
;; (a-macro (+ 1 2))

(type (Tree a) (Node a a) (Leaf a))

;; (fun (glah (Node a a n))
;;      (list 'node n))
;; (fun (glah (Leaf a l))
;;      (list 'leaf l))

(fun (bluh x (Tree (Hoop a) t))
     (glah t))

;; (bluh 10 (Tree (Leaf 20)))
