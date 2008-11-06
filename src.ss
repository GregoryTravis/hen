(fun (env-lookup v (Env)) NoSuchValue)
(fun (env-lookup v (Env (Binding vv value) . rest))
     (if (== v vv)
         value
         (env-lookup v (Env . rest))))

(fun (evl (Constant c) env) (Constant c))
(fun (evl (Var v) env)
     (env-lookup v env))
(fun (evl (Lambda var exp) env)
     (Closure (Lambda var exp) env))

(fun (bonk . stuff) (App2 . stuff))

;; (fun (evl-list (Cons a d) env)
;;      (Cons (evl a env) (evl-list d env)))
;; (fun (evl-list Nil env)
;;      Nil)

(fun (evl (App f arg) env)
     (evl (App2 (evl f env) (evl arg env)) env))

;(evl (App (Lambda (Var 'v) 'v) (Constant 1)) (Env))
;(evl-list (list (Lambda (Var 'v) 'v)) (Env))

(fun (evl (App2 (Primitive '+) (Cons (Constant a) (Cons (Constant b) Nil))) env)
     (Constant (+  a b)))

(fun (evl (App2 (Closure (Lambda (Var v) body) (Env . bindings)) arg) env)
     (evl body (Env (Binding v (evl arg env)) . bindings)))

;; (fun (evl (App (Lambda pat body) arg) env)
;;      (evl (App (evl (Lambda pat body) env) arg) env))

(fun (evl (App2 (Closure (Lambda (Constant c) body) env) (Constant cc)) env)
     (if (== c cc)
         (evl body env)
         Fail))

(fun (evl (App2 (Closure (Lambda (Cons a d) body) closure-env) (Cons aa dd)) env)
     (evl (App (Lambda a (App (Lambda d body) dd)) aa) closure-env))

(fun (evl (App2 (Var v) . args) env)
     (evl (App2 (env-lookup v env) . args) env))

;; (fun (evl (App (Primitive +) (Constant a) (Constant b)) env)
;;      (Constant (+ a b)))

(fun (evl (Cons a b) env) (Cons (evl a env) (evl b env)))
(fun (evl Nil env) Nil)

;(evl (App (Var 'plus) (Cons (Constant 1) (Cons (Constant 2) Nil))) (Env (Binding 'plus (Primitive '+))))

(evl (Constant 2) (Env))
(evl (Var 'a) (Env (Binding 'a (Constant 10))))
(evl (Lambda (Var 'v) 'a) (Env (Binding 'a (Constant 10))))

(evl (App2 (Closure (Lambda (Var 'v) (Var 'a)) (Env (Binding 'a (Constant 10)))) (Constant 20)) (Env))
(evl (App (Lambda (Var 'v) (Constant 30)) (Constant 20)) (Env))

(evl (App2 (Closure (Lambda (Constant 10) (Var 'a)) (Env (Binding 'a (Constant 20)))) (Constant 10)) (Env))
(evl (App2 (Closure (Lambda (Constant 10) (Var 'a)) (Env (Binding 'a (Constant 20)))) (Constant 30)) (Env))

(evl (App2 (Closure (Lambda (Cons (Var 'a) (Var 'b)) (Var 'b)) (Env)) (Cons (Constant 1) (Constant 2))) (Env))
(evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'a)) (Cons (Constant 1) (Constant 2))) (Env))
(evl (App (Lambda (Cons (Var 'a) (Var 'b)) (Var 'b)) (Cons (Constant 1) (Constant 2))) (Env))

;; (evl (App (Var 'plus) (Constant 1) (Constant 2))
;;      (Env (Binding 'plus (Primitive +))))


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
