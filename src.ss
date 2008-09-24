(fun (guardo-a a b)
     (? true)
     (Bar a a))

(fun (guardo-b a b)
     (? false)
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
