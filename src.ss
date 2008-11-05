(fun (env-lookup v (Env)) NoSuchValue)
(fun (env-lookup v (Env (Binding vv value) . rest))
     (if (== v vv)
         value
         (env-lookup v (Env . rest))))

(fun (evl (Constant c) env) (Constant c))
(fun (evl (App (+ (Constant a) (Constant b))) env) (Constant (+ a b)))
(fun (evl (Var v) env)
     (env-lookup v env))
(fun (evl (Lambda var exp) env)
     (Closure (Lambda var exp) env))
(fun (evl (App (Closure (Lambda (Var v) body) (Env . bindings)) arg) env)
     (evl body (Env (Binding v (evl arg env)) . bindings)))

(fun (evl (App (Closure (Lambda (Constant c) body) env) (Constant cc)) env)
     (if (== c cc)
         (evl body env)
         Fail))

(fun (evl (App (Closure (Lambda (Cons a d) body) closure-env) (Cons aa dd)) env)
     (evl (App (Lambda a (App (Lambda d body) dd)) aa) closure-env))

(fun (evl (App f arg) env)
     (evl (App (evl f env)
               (evl arg env)) env))

(fun (evl (Cons a b) env) (Cons (evl a env) (evl b env)))

(fun (evl (App (Primitive f) . args) env)
     (f . args))

(evl (App (Primitive +) 2 3) (Env))

;; (evl (Constant 2) (Env))
;; (evl (App (list + (Constant 2) (Constant 3))) (Env))
;; (evl (Var 'a) (Env (Binding 'a (Constant 10))))
;; (evl (Lambda (Var 'v) 'a) (Env (Binding 'a (Constant 10))))

;; (evl (App (Closure (Lambda (Var 'v) (Var 'a)) (Env (Binding 'a (Constant 10)))) (Constant 20)) (Env))
;; (evl (App (Lambda (Var 'v) (Constant 30)) (Constant 20)) (Env))

;; (evl (App (Closure (Lambda (Constant 10) (Var 'a)) (Env (Binding 'a (Constant 20)))) (Constant 10)) (Env))
;; (evl (App (Closure (Lambda (Constant 10) (Var 'a)) (Env (Binding 'a (Constant 20)))) (Constant 30)) (Env))

;; (evl (App (Closure (Lambda (Cons (Var 'a) (Var 'b)) (Var 'b)) (Env)) (Cons (Constant 1) (Constant 2))) (Env))
;; (evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Cons (Constant 1) (Constant 2))) (Env))
;; (evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'b)) (Cons (Constant 1) (Constant 2))) (Env))


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
