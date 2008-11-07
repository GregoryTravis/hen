(fun (env-lookup v (Env)) NoSuchValue)
(fun (env-lookup v (Env (Binding vv value) . rest))
     (if (== v vv)
         value
         (env-lookup v (Env . rest))))

;; Constants.
(fun (evl (Constant c) env) (Constant c))

;; Variables.
(fun (evl (Var v) env)
     (env-lookup v env))

;; Lambda expresion.
(fun (evl (Lambda var exp) env)
     (Closure (Lambda var exp) env))

;; Function call.
(fun (evl (App f arg) env)
     (apply-fun (evl f env) (evl arg env)))

(fun (apply-fun f arg)
     (must (apply-fun-or-fail f arg)))

;; Primitive function call
(fun (apply-fun-or-fail (Primitive '+) (Cons (Constant a) (Cons (Constant b) Nil)))
     (Constant (+  a b)))

;; Pattern lambda function call
(fun (apply-fun-or-fail (Closure (Lambda (Constant c) body) env) (Constant cc))
     (if (== c cc)
         (Yup (evl body env))
         Nope))
(fun (apply-fun-or-fail (Closure (Lambda (Var v) body) (Env . bindings)) arg)
     (Yup (evl body (Env (Binding v arg) . bindings))))
(fun (apply-fun-or-fail (Closure (Lambda (Cons a d) body) closure-env) (Cons aa dd))
     (Yup (evl (App (Lambda a (App (Lambda d body) dd)) aa) closure-env)))
(fun (apply-fun-or-fail (Closure (Lambda pat body) closure-env) target)
     Nope)

(fun (must (Yup a)) a)
(fun (must Nope) (err 'pattern-match-failure))

;; Data.
(fun (evl (Cons a b) env) (Cons (evl a env) (evl b env)))
(fun (evl Nil env) Nil)

;(evl (App (Lambda (Cons (Var 'a) (Var 'b)) 'a) (Constant 10)) (Env))

(evl (Constant 2) (Env))
(evl (Var 'a) (Env (Binding 'a (Constant 10))))
(evl (Lambda (Var 'v) 'a) (Env (Binding 'a (Constant 10))))

(evl (App (Lambda (Var 'v) (Var 'a)) (Constant 20)) (Env (Binding 'a (Constant 10))))
(evl (App (Lambda (Var 'v) (Constant 30)) (Constant 20)) (Env))

(evl (App (Lambda (Constant 10) (Var 'a)) (Constant 10)) (Env (Binding 'a (Constant 20))))
;(evl (App (Lambda (Constant 10) (Var 'a)) (Constant 30)) (Env (Binding 'a (Constant 20))))

(evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Cons (Constant 1) (Constant 2))) (Env))
(evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'b)) (Cons (Constant 1) (Constant 2))) (Env))


;; (env-lookup 'a (Env))
;; (env-lookup 'a (Env (Binding 'a 10)))
;; (env-lookup 'a (Env (Binding 'b 10)))
;; (env-lookup 'a (Env (Binding 'a 10) (Binding 'b 10)))
;; (env-lookup 'a (Env (Binding 'b 10) (Binding 'a 10)))

;; (evl (Var 'a) (Env))
;; (evl (Var 'a) (Env (Binding 'a 10)))
;; (evl (Var 'a) (Env (Binding 'b 10)))
;; (evl (Var 'a) (Env (Binding 'a 10) (Binding 'b 10)))
;; (evl (Var 'a) (Env (Binding 'b 10) (Binding 'a 10)))

;; (fun (maybe-append False bindings) False)
;; (fun (maybe-append bindings False) False)

;; (fun (match (Cons a b) (Cons c d))
;;      (maybe-append (match a c) (match b d)))
;; (fun (match (Var v) x)
;;      (list (Binding v x)))
