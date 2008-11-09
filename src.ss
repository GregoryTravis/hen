(fun (env-lookup v (Env)) NoSuchValue)
(fun (env-lookup v (Env (Binding vv value) . rest))
     (if (== v vv)
         value
         (env-lookup v (Env . rest))))
(fun (env-lookup v (Sequence a b))
     (goober v (env-lookup v a) b))
(fun (goober v NoSuchValue b)
     (env-lookup v b))
(fun (goober v vv b)
     vv)

;; Utilities.
(fun (cons a d) (list a . d))
(fun (evl-list (a . d) env genv)
     (cons (evl a env genv) (evl-list d env genv)))
(fun (evl-list () env genv) (list))

;; Constants.
(fun (evl (Constant c) env genv) (Constant c))

;; Variables.
(fun (evl (Var v) env genv)
     (env-lookup v (Sequence env genv)))

;; Lambda expresion.
(fun (evl (Lambda var exp) env genv)
     (Closure (Lambda var exp) env))

;; Function call.
(fun (evl (App f arg) env genv)
     (apply-fun (evl f env genv) (evl arg env genv) genv))
(fun (evl (NiceApp f arg) env genv)
;     (glook (evl f env genv) arg env genv)) 
     (apply-fun-or-fail (evl f env genv) (evl arg env genv) genv))
;; (fun (glook Fail arg env genv) Fail)
;; (fun (glook (Yup v) arg env genv)
;;      (apply-fun-or-fail v (evl arg env genv) genv))

(fun (apply-fun f arg genv)
     (must (apply-fun-or-fail f arg genv)))

;; Primitive function call
(fun (apply-fun-or-fail (Primitive '+) (Cons (Constant a) (Cons (Constant b) Nil)) genv)
     (Yup (Constant (+ a b))))

;; Pattern lambda function call
(fun (apply-fun-or-fail (Closure (Lambda (Constant c) body) env) (Constant cc) genv)
     (if (== c cc)
         (Yup (evl body env genv))
         Nope))
(fun (apply-fun-or-fail (Closure (Lambda (Var v) body) (Env . bindings)) arg genv)
     (Yup (evl body (Env (Binding v arg) . bindings) genv)))
(fun (apply-fun-or-fail (Closure (Lambda (Cons a d) body) closure-env) (Cons aa dd) genv)
;     (evl (App (Lambda a (NiceApp (Lambda d body) dd)) aa) closure-env genv))
     (vink (evl (NiceApp (Lambda a (NiceApp (Lambda d body) dd)) aa) closure-env genv)))
(fun (vink (Yup (Yup v))) (Yup v))
(fun (vink (Yup Nope)) Nope)
(fun (vink Nope) Nope)

(fun (apply-fun-or-fail (Closure (Lambda pat body) closure-env) target genv)
     Nope)
(fun (apply-fun-or-fail (LLambda clo . clos) arg genv)
     (joof (apply-fun-or-fail clo arg genv)
           (LLambda . clos) arg genv))
(fun (joof Nope ll arg genv) (apply-fun-or-fail ll arg genv))
(fun (joof (Yup a) ll arg genv) (Yup a))
;     (this-or-that (apply-fun-or-fail clo arg genv)
;                   (apply-fun-or-fail (LLambda . clos) arg genv)))
(fun (apply-fun-or-fail (LLambda) arg genv)
     Nope)

;(fun (this-or-that (Yup a) b) (Yup a))
;(fun (this-or-that Nope b) b)

(fun (brap l) (LLambda . l))

;; Multi-lambda.
(fun (evl (LLambda . clos) env genv)
     (brap (evl-list clos env genv)))

(fun (must (Yup a)) a)
(fun (must Nope) (err 'pattern-match-failure))

;; Data.
(fun (evl (Cons a b) env genv) (Cons (evl a env genv) (evl b env genv)))
(fun (evl Nil env genv) Nil)

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
