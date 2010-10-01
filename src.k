(fun (map 'f Nil) Nil)
(fun (map 'f (Cons 'a 'd)) (Cons ('f 'a) (map 'f 'd)))

(fun (swap (P 'a 'b)) (P 'b 'a))

;(fun (main) (map swap (Cons (P A B) (Cons (P C D) Nil))))

(fun (parse-exp 'e)
     (if (app? 'e)
         (App (map parse-exp 'e))
         (if (var? 'e)
             (Var 'e)
             (if (data? 'e)
                 (Data 'e)
                 Error))))

(fun (parse-rule (Cons fun (Cons 'pattern (Cons 'body Nil))))
     (Rule (parse-exp 'pattern) (parse-exp 'body)))

(fun (parse-program 'rules) (map parse-rule 'rules))

(fun (main) (parse-program (current-program)))
