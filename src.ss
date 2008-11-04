(fun (evl (App (+ a b))) (+ a b))
(fun (evl a) a)

(evl 2)
(evl (App (list + 2 3)))
