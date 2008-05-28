;; (load "h.ss")
(load "lib.ss")
(load "primitives.ss")

(define (pat-ok? p)
  (if (pair? p)
      (and
       (proper-list? p)
       (if (eq? (car p) 'quote)
           (is-quote? p)
           (and
            (atom? (car p))
            (all (map pat-ok? (cdr p))))))
      #t))

(define (simplify-list-cdr d pat-p)
  (if (pair? d)
      (list 'pair (simplify (car d) pat-p) (simplify-list-cdr (cdr d) pat-p))
      (if (null? d)
          (list 'nil)
          (err d))))

(define (simplify-list p pat-p)
  (list 'pair (list 'literal (car p)) (simplify-list-cdr (cdr p) pat-p)))

(define (simplify p pat-p)
  (assert (pat-ok? p) p)
  (cond
   ((is-quote? p) (if pat-p
                      (list 'literal (quote-quoted p))
                      (err 'quote-outside-pattern p pat-p)))
   ((pair? p) (simplify-list p pat-p))
   ((symbol? p) (if pat-p (list 'var p) (list 'literal p)))
   (#t (list 'literal p))))

(define (simplify-pat p) (simplify p #t))
(define (simplify-exp p) (simplify p #f))

(define (match p t)
  (let ((p-what (car p))
        (t-what (car t)))
    (cond
     ((and (eq? p-what 'nil)
           (eq? t-what 'nil))
      (just '()))
     ((and (eq? p-what 'pair)
           (eq? t-what 'pair))
      (maybe-append (match (cadr p) (cadr t))
                    (match (caddr p) (caddr t))))
     ((and (eq? p-what 'literal)
           (eq? t-what 'literal))
      (if
       (equal? (cadr p) (cadr t))
       (just '())
       fail))
     ((eq? p-what 'var)
      (just (list (list 'binding (cadr p) t))))
     (#t fail))))

(define (look-up-binding env varname)
  (if (null? env)
      fail
      (if (eq? varname (cadar env))
          (just (caddar env))
          (look-up-binding (cdr env) varname))))

(define (apply-match-env env body)
  (let ((b-what (car body)))
    (cond
     ((eq? b-what 'nil) body)
     ((eq? b-what 'pair)
      (list 'pair (apply-match-env env (cadr body)) (apply-match-env env (caddr body))))
     ((eq? b-what 'literal) body)
     ((eq? b-what 'var)
      (let ((binding (look-up-binding env (cadr body))))
        (if (fail? binding)
            (err 'unbound env body)
            (just-value binding))))
     (#t (err 'apply-match-env env body)))))

(define (hshew . args)
  (apply shew (map unsimplify args)))

(define (unsimplify o)
  (cond
   ((is-quote? o) o)
   ((eq? (car o) 'pair)
    (cons (unsimplify (cadr o))
          (unsimplify (caddr o))))
   ((eq? (car o) 'literal)
    (cadr o))
   ((eq? (car o) 'nil) '())
   ((eq? (car o) 'var) (cadr o))
   (#t (err 'unsimplify o))))

(define (fun? p)
  (and (pair? p)
       (or (eq? (car p) 'fun) (eq? (car p) 'macro))
       (and (= 3 (length p)))))

(define (type-declaration? p)
  (and
   (proper-list? p)
   (eq? 'type (car p))
   (none (map atom? (cdr p)))))

(define (macro? p)
  (and (fun? p) (eq? 'macro (car p))))

(define (run-file f)
  (run-file-src (read-objects f)))

(define rewrite-rules '())
(define macros '())

(define (define-rule o)
  (assert (fun? o))
  (let* ((pat (cadr o))
         (body (caddr o))
         (simplified-rule (list (simplify-pat pat) (simplify-pat body))))
    (if (macro? o)
        (set! macros (snoc macros simplified-rule))
        (set! rewrite-rules (snoc rewrite-rules simplified-rule)))
    (define-type-promoters o)
    (display "* ")
    (shew o)))

(define (try-to-rewrite-funlikes funlikes simplified-e)
  (find-first-maybe
   (lambda (rewrite)
     (let ((simplified-pat (car rewrite))
           (simplified-body (cadr rewrite)))
       ((maybe-compose
         (lambda () (match simplified-pat simplified-e))
         (lambda (env) (just (apply-match-env env simplified-body)))))))
   funlikes))

(define (try-to-rewrite-funs simplified-e)
  (try-to-rewrite-funlikes rewrite-rules simplified-e))
(define (try-to-rewrite-macros simplified-e)
  (try-to-rewrite-funlikes macros simplified-e))

(define (try-to-rewrite-primitives e)
  (let ((e (unsimplify e)))
    (let ((f (car e))
          (args (cdr e)))
      (if (symbol? f)
          ((maybe-compose
            (lambda () (get-primitive f))
            (lambda (prim-fun) (just (simplify-exp (apply prim-fun args))))))
          fail))))

(define (try-to-rewrite e)
  (let ((try-funs (try-to-rewrite-funs e)))
    (if (just? try-funs)
        try-funs
        (try-to-rewrite-primitives e))))

(define (normal-form-kids e)
  (cond
   ((eq? 'pair (car e))
    (list 'pair (normal-form (cadr e)) (normal-form-kids (caddr e))))
   ((eq? 'nil (car e)) '(nil))
   (#t (err normal-form-kids e))))

(define (normal-form e)
  (assert (pair? e) e)
  (cond
   ((or (eq? 'literal (car e))
        (eq? 'nil (car e)))
    e)
   ((eq? 'pair (car e))
    (let ((guh (try-to-rewrite-macros e)))
      (if (just? guh)
          (normal-form (just-value guh))
          (let ((ee (normal-form-kids e)))
            (let ((rewrite-maybe (try-to-rewrite ee)))
              (if (fail? rewrite-maybe)
                  ee
                  (normal-form (just-value rewrite-maybe))))))))
   (#t (err 'normal-form e))))

(define (exec-exp e)
  (let ((oe e)
        (e (simplify-exp e)))
    (display "+ ")
    (hshew e)
    (display "    =>\n")
    (let ((nf (normal-form e)))
      (display "  ")
      (hshew nf))))

(define base-type-promoters '())

(define (note-type-declaration t)
  (set! base-type-promoters (append base-type-promoters
                               (gen-base-type-promoters t))))

(define (gen-base-type-promoters t)
  (assert (type-declaration? t))
  (let ((type-pat (cadr t))
        (ctors (cddr t)))
    (map (lambda (ctor)
           (list type-pat ctor))
         ctors)))

(define (exec-top-level-form o)
  (cond
   ((type-declaration? o) (note-type-declaration o))
   ((fun? o) (define-rule o))
   (#t (exec-exp o))))

(define (run-file-src forms)
  (map exec-top-level-form forms))

(define (define-type-promoters-for-type rule promoter)
  (shew 'oik rule promoter))

(define (define-type-promoters rule)
  (map (lambda (promoter)
         (define-type-promoters-for-type rule promoter))
       base-type-promoters))

;(tracefun match apply-match-env)
;(tracefun simplify simplify-exp simplify-pat simplify-list simplify-list-cdr)
;(tracefun is-quote? pat-ok?)
;(tracefun get-primitive try-to-rewrite-primitives)
;(tracefun maybe-compose maybe-try)
