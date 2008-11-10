;; (fun (vort (Boo a . d))
;;      (Cons a (Cons a (vort (Boo . d)))))
;; (fun (vort (Boo)) Nil)

;; (vort (Boo 1 2 3))

;; (fun (vort (a . d))
;;      (Cons a (Cons a (vort d))))
;; (fun (vort ()) Nil)

;; (vort (list 1 2 3))

;; (fun (evl (QLet ((B var value) . bindings) body) env genv)
;;      (evl (App (Lambda var (QLet bindings body)) value) env genv))
;; (fun (evl (QLet () body) env genv)
;;      (evl body env genv))

;; (evl (QLet (list (B (Var 'a) (Constant 100))) (Var 'a)) (Env) (Env))
