(fun (bif 'true t e) t)
(fun (bif 'false t e) e)

(bif (= 1 2) right wrong)
(bif (= 1 1) right wrong)
