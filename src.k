(fun (all-true (Cons True 'rest)) (all-true 'rest))
(fun (all-true (Cons '_ 'rest)) (Fail))
(fun (all-true Nil) (ok))

;; (fun (parse (fun ('f . 'args))) (Fun 'f (parse-args 'args)))
;; (fun (parse (quote 's)) (Quote 's))
;; (fun (parse-args 'args) 'args)
;; (fun (parse ['a . 'd]) [(parse 'a) . (parse 'd)])
;; (fun (parse Nil) Nil)
(fun (parse 'a) 'a)

(fun (main) (parse (current-program)))

(fun (main) [(all-true [True True True])
             (all-true [True False True])])
