(fun (all-true (Tests True . 'rest)) (all-true (Tests . 'rest)))
(fun (all-true (Tests)) (ok))
(fun (all-true _) (Fail))
(fun (main) (all-true (Test True True True)))
