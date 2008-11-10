(fun (env-lookup v (Env)) NoSuchValue)
(fun (env-lookup v (Env (Binding vv value) . rest))
     (if (== v vv)
         value
         (env-lookup v (Env . rest))))
(fun (env-lookup v (Sequence a b))
     (let ((ea (env-lookup v a)))
       (if (== ea NoSuchValue)
           (env-lookup v b)
           ea)))

;; Utilities.
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
     (apply-fun-or-fail (evl f env genv) (evl arg env genv) genv))
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
     (@ (evl (NiceApp (Lambda a (NiceApp (Lambda d body) dd)) aa) closure-env genv)
        (Yup a) a
        Nope Nope))

(fun (apply-fun-or-fail (Closure (Lambda pat body) closure-env) target genv)
     Nope)
(fun (apply-fun-or-fail (LLambda clo . clos) arg genv)
     (@ (apply-fun-or-fail clo arg genv)
        Nope (apply-fun-or-fail (LLambda . clos) arg genv)
        (Yup a) (Yup a)))
(fun (apply-fun-or-fail (LLambda) arg genv)
     Nope)

(fun (brap l) (LLambda . l))

;; Multi-lambda.
(fun (evl (LLambda . clos) env genv)
     (brap (evl-list clos env genv)))

;; Data.
(fun (evl (Cons a b) env genv) (Cons (evl a env genv) (evl b env genv)))
(fun (evl Nil env genv) Nil)

(fun (evl (Let ((B var value) . bindings) body) env genv)
     (evl (App (Lambda var (Let bindings body)) value) env genv))
(fun (evl (Let () body) env genv)
     (evl body env genv))
