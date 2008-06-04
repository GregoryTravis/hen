;; (load "h.ss")
(load "lib.ss")
(load "primitives.ss")

(define verbose-show-defined-rules #f)

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

(define (quote-first-maybe e)
  (if (and (not (is-quote? e)) (pair? e) (symbol? (car e)))
      (cons `(quote ,(car e)) (cdr e))
      e))

(define (un-quote-first-maybe e)
  (if (and (pair? e) (is-quote? (car e)) (symbol? (quote-quoted (car e))))
      (cons (quote-quoted (car e)) (cdr e))
      e))

(define (preprocess e)
  (cond
   ((is-quote? e) e)
   ((pair? e)
    (map preprocess (quote-first-maybe e)))
   (#t e)))

(define (un-preprocess e)
  (cond
   ((pair? e)
    (map un-preprocess (un-quote-first-maybe e)))
   (#t e)))

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
  (apply shew (map un-preprocess args)))

(define (fun? p)
  (and (pair? p)
       (or (eq? (car p) 'fun) (eq? (car p) 'macro))
       (and (= 3 (length p)))))

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
         (rule (list pat body)))
    (if (macro? o)
        (set! macros (snoc macros rule))
        (set! rewrite-rules (snoc rewrite-rules rule)))
    (if verbose-show-defined-rules
        (begin (display "* ") (shew o)))))

(define (try-to-rewrite-funlikes funlikes e)
  (find-first-maybe
   (lambda (rewrite)
     (let ((pat (car rewrite))
           (body (cadr rewrite)))
       ((maybe-compose
         (lambda () (match pat e))
         (lambda (env) (just (apply-match-env env body)))))))
   funlikes))

(define (try-to-rewrite-funs e)
  (try-to-rewrite-funlikes rewrite-rules e))
(define (try-to-rewrite-macros e)
  (try-to-rewrite-funlikes macros e))

(define (try-to-rewrite-primitives e)
  (let ((f (car e))
        (args (cdr e)))
    (if (symbol? f)
        ((maybe-compose
          (lambda () (get-primitive f))
          (lambda (prim-fun) (just (orthogonalize-exp (apply prim-fun args))))))
        fail)))

(define (try-to-rewrite e)
  (let ((try-funs (try-to-rewrite-funs e)))
    (if (just? try-funs)
        try-funs
        (try-to-rewrite-primitives e))))

(define (normal-form-kids e)
  (assert (proper-list? e))
  (cond
   ((null? e) e)
   ((pair? e)
    (cons (normal-form (car e))
          (normal-form-kids (cdr e))))
   (#t (err normal-form-kids e))))

(define (normal-form e)
  (cond
   ((literal? e) e)
   ((pair? e)
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
  (let ((e (preprocess e)))
    (display "+ ")
    (hshew e)
    (display "    =>\n")
    (let ((nf (normal-form e)))
      (display "  ")
      (hshew nf))))

(define (exec-top-level-form o)
  (if (fun? o)
      (define-rule o)
      (exec-exp o)))

(define (run-file-src forms)
  (map exec-top-level-form forms))

;(tracefun match apply-match-env)
;(tracefun orthogonalize orthogonalize-exp orthogonalize-pat orthogonalize-list orthogonalize-list-cdr)
;(tracefun is-quote? pat-ok?)
;(tracefun get-primitive try-to-rewrite-primitives)
;(tracefun maybe-compose maybe-try)
;(tracefun normal-form normal-form-kids)
;(tracefun preprocess un-preprocess quote-first-maybe un-quote-first-maybe)
