(evl (App
      (LLambda
       (Lambda (Constant 10) (Constant 1)) (Lambda (Cons (Var 'a) (Constant 20)) (Constant 800))
       (Lambda (Cons (Constant 10) (Var 'a)) (Constant 900)) (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Lambda (Var 'a) (Constant 1000)))
      (Constant 10))
     (Env (Binding 'c (Constant 10)))
     (Env))

(evl (App
      (LLambda
       (Lambda (Constant 10) (Constant 1)) (Lambda (Cons (Var 'a) (Constant 20)) (Constant 800))
       (Lambda (Cons (Constant 10) (Var 'a)) (Constant 900)) (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Lambda (Var 'a) (Constant 1000)))
      (Cons (Constant 100) (Constant 20)))
     (Env (Binding 'c (Constant 10)))
     (Env))

(evl (App
      (LLambda
       (Lambda (Constant 10) (Constant 1)) (Lambda (Cons (Var 'a) (Constant 20)) (Constant 800))
       (Lambda (Cons (Constant 10) (Var 'a)) (Constant 900)) (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Lambda (Var 'a) (Constant 1000)))
      (Cons (Constant 200) (Constant 40)))
     (Env (Binding 'c (Constant 10)))
     (Env))

(evl (App
      (LLambda
       (Lambda (Constant 10) (Constant 1)) (Lambda (Cons (Var 'a) (Constant 20)) (Constant 800))
       (Lambda (Cons (Constant 10) (Var 'a)) (Constant 900)) (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Lambda (Var 'a) (Constant 1000)))
      (Cons (Constant 10) (Constant 600)))
     (Env (Binding 'c (Constant 10)))
     (Env))

(evl (App
      (LLambda
       (Lambda (Constant 10) (Constant 1)) (Lambda (Cons (Var 'a) (Constant 20)) (Constant 800))
       (Lambda (Cons (Constant 10) (Var 'a)) (Constant 900)) (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Lambda (Var 'a) (Constant 1000)))
      (Constant 500))
     (Env (Binding 'c (Constant 10)))
     (Env))

;(evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Constant 10)) (Env) (Env))

(evl (Constant 2) (Env) (Env))
(evl (Var 'a) (Env (Binding 'a (Constant 10))) (Env))
(evl (Lambda (Var 'v) 'a) (Env (Binding 'a (Constant 10))) (Env))

(evl (App (Lambda (Var 'v) (Var 'a)) (Constant 20)) (Env (Binding 'a (Constant 10))) (Env))
(evl (App (Lambda (Var 'v) (Constant 30)) (Constant 20)) (Env) (Env))

(evl (App (Lambda (Constant 10) (Var 'a)) (Constant 10)) (Env (Binding 'a (Constant 20))) (Env))
;(evl (App (Lambda (Constant 10) (Var 'a)) (Constant 30)) (Env (Binding 'a (Constant 20))) (Env))

(evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Cons (Constant 1) (Constant 2))) (Env) (Env))
(evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'b)) (Cons (Constant 1) (Constant 2))) (Env) (Env))


(env-lookup 'a (Env))
(env-lookup 'a (Env (Binding 'a 10)))
(env-lookup 'a (Env (Binding 'b 10)))
(env-lookup 'a (Env (Binding 'a 10) (Binding 'b 10)))
(env-lookup 'a (Env (Binding 'b 10) (Binding 'a 10)))

(evl (Var 'a) (Env) (Env))
(evl (Var 'a) (Env (Binding 'a 10)) (Env))
(evl (Var 'a) (Env (Binding 'b 10)) (Env))
(evl (Var 'a) (Env (Binding 'a 10) (Binding 'b 10)) (Env))
(evl (Var 'a) (Env (Binding 'b 10) (Binding 'a 10)) (Env))

(fun (maybe-append False bindings) False)
(fun (maybe-append bindings False) False)

(fun (match (Cons a b) (Cons c d))
     (maybe-append (match a c) (match b d)))
(fun (match (Var v) x)
     (list (Binding v x)))

(env-lookup 'a (Sequence (Env (Binding 'a (Constant 10)) (Binding 'b (Constant 20))) (Env (Binding 'c (Constant 100)) (Binding 'd (Constant 200)))))
(env-lookup 'b (Sequence (Env (Binding 'a (Constant 10)) (Binding 'b (Constant 20))) (Env (Binding 'c (Constant 100)) (Binding 'd (Constant 200)))))
(env-lookup 'c (Sequence (Env (Binding 'a (Constant 10)) (Binding 'b (Constant 20))) (Env (Binding 'c (Constant 100)) (Binding 'd (Constant 200)))))
(env-lookup 'd (Sequence (Env (Binding 'a (Constant 10)) (Binding 'b (Constant 20))) (Env (Binding 'c (Constant 100)) (Binding 'd (Constant 200)))))

(evl (Let (list (B (Var 'a) (Constant 100))) (Var 'a)) (Env) (Env))

(evl (Match (Constant 2) (Constant 2) (Constant 3) (Constant 4) (Constant 5)) (Env) (Env))
(evl (Match (Constant 4) (Constant 2) (Constant 3) (Constant 4) (Constant 5)) (Env) (Env))
