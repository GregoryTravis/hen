(load "lib.ss")

(define src
  '(
    (fun (boot (Cons a Nil)) (Cons a (Cons a Nil)))
    ))

(define (rewrite e src)
  (let ((new-e (rewrite-this e src)))
    (if (equal? new-e e)
        (if (pair? new-e)
            (cons (rewrite (car new-e) src) (rewrite (cdr new-e) src))
            new-e)
        (rewrite e src))))

(define (rewrite-this e src)
  (if (null? src)
      e
      (mtch src
            (('fun pat body) . the-rest)
            (mtch (match-maybe e pat)
                  'fail e
                  ('just bindings) (apply-bindings body bindings)))))

(define (match-maybe e pat)
  (cond
   ((and (pair? e) (pair? pat)) (maybe-append (match-maybe (car e) (car pat))
                                              (match-maybe (cdr e) (cdr pat))))
   ((ctor? e) (if (eq? e pat) (just '()) fail))
   ((and (null? e) (null? pat)) (just '()))
   (#t (just `((,pat . ,e))))))

(define (apply-bindings e bindings)
  (cond
   ((pair? e) (cons (apply-bindings (car e) bindings) (apply-bindings (cdr e) bindings)))
   ((ctor? e) e)
   ((null? e) e)
   ((symbol? e) (lookup e bindings))
   (#t (err 'unbound e bindings))))

;; ; tests
;; '(
;;   ((bindings-maybe '(a B R c e) '(d B R f g))) (just ((d a) (f c) (g e)))
;;   ((bindings-maybe '(a B R c e) '(d B Rr f g)) fail)
;;   ((map ($ apply-bindings _ '((d a) (f c) (g e))) '(d f g Joe (d f g Joe)))
;;    (a c e Joe (a c e Joe)))
;;   ((rewrite-this '(boot (Cons q Nil)) src)
;;    (Cons q (Cons q Nil)))
;;   )

(rewrite-this '(boot (Cons q Nil)) src)
