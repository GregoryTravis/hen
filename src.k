(fun (all-true True . 'rest) (all-true . 'rest))
;; (fun (all-true) (ok))
;; (fun (all-true _) (Fail))

(fun (main) (current-program))
