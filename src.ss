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
;(fun (evl (App (Closure (

;; (fun (evl (App (Closure (Lambda (Cons a b) body) env)
;;                (Cons c d)))
;;      (App (Closure 
;; (fun (match (Var v) x) (list (Binding v x)))
;; (fun (cons-env bindings) (Env bindings))
;; (fun (evl (App (Closure (Lambda pat body) (Env . bindings)) target) env)
;;      (evl body (cons-env (append (match pat target) bindings))))
;; ;     (evl body (Env . (append (match pat target) bindings))))
;; ;     (evl body (Env . bindings)))

;(fun (evl a env) a)

(evl (Constant 2) (Env))
(evl (App (list + (Constant 2) (Constant 3))) (Env))
(evl (Var 'a) (Env (Binding 'a (Constant 10))))
(evl (Lambda (Var 'v) 'a) (Env (Binding 'a (Constant 10))))
;; (evl (App (Closure (Lambda (Var 'v) (Var 'a)) (Env (Binding 'a (Constant 10)))) (Constant 20)) (Env))
;; (evl (App (Closure (Lambda (Constant 10) (Var 'a)) (Env (Binding 'a (Constant 10)))) (Constant 10)) (Env))

;; (evl (App (Closure (Lambda 'v (Var 'v)) (Env (Binding 'a 10))) 20) (Env))

;(evl (Lambda var exp))

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
