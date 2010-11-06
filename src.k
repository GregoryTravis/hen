(fun (all-true True . 'rest) (all-true . 'rest))
;; (fun (all-true) (ok))
;; (fun (all-true _) (Fail))

;; (fun (parse (fun ('f . 'args))) (Fun 'f (parse-args 'args)))
;; (fun (parse (quote 's)) (Quote 's))
;; (fun (parse-args 'args) 'args)
;; (fun (parse ['a . 'd]) [(parse 'a) . (parse 'd)])
;; (fun (parse Nil) Nil)
(fun (parse 'a) 'a)

(fun (main) (parse (current-program)))
