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

(define (auto-quote-ctor e)
  (cond
   ((and (not (is-quote? e)) (pair? e) (symbol? (car e)))
    (map auto-quote-ctor (cons `(quote ,(car e)) (cdr e))))
   ((pair? e)
    (map auto-quote-ctor e))
   (#t e)))

(define (un-auto-quote-ctor e)
  (cond
   ((and (pair? e) (is-quote? (car e)) (symbol? (quote-quoted (car e))))
    (map un-auto-quote-ctor (cons (quote-quoted (car e)) (cdr e))))
   ((pair? e)
    (map un-auto-quote-ctor e))
   (#t e)))

(define (add-missing-guard e)
  (if (fun-without-guard? e)
      `('fun ,(cadr e) ('? 'true) ,(caddr e))
      e))

(define (preprocess e)
  (add-missing-guard (auto-quote-ctor e)))

(define (un-preprocess e)
  (un-auto-quote-ctor e))

(define (match p t)
  (cond
   ((literal? p)
    (if (equal? p t)
        (just '())
        fail))
   ((symbol? p) (just (list (cons p t))))
   ((pair? p)
    (maybe-apply append (maybe-zip match p t)))
   (#t (err))))

(define (look-up-binding env varname)
  (if (null? env)
      fail
      (if (eq? varname (caar env))
          (just (cdar env))
          (look-up-binding (cdr env) varname))))

(define (apply-match-env env body)
  (cond
   ((literal? body) body)
   ((pair? body) (map (lambda (subexp) (apply-match-env env subexp)) body))
   ((symbol? body) (just-value (look-up-binding env body)))
   (#t (err))))

(define (hshew . args)
  (apply shew (map un-preprocess args)))

(define (guard? e)
  (and (= 2 (length e)) (equal? ''? (car e))))

(define (fun-with-guard? p)
  (and (pair? p)
       (or (equal? (car p) ''fun) (equal? (car p) ''macro))
       (= 4 (length p))
       (guard? (caddr p))))

(define (fun-without-guard? p)
  (and (pair? p)
       (or (equal? (car p) ''fun) (equal? (car p) ''macro))
       (= 3 (length p))))

(define (fun? p)
  (or (fun-with-guard? p) (fun-without-guard? p)))

(define (macro? p)
  (and (fun? p) (eq? 'macro (car p))))

(define (run-file f)
  (run-file-src (read-objects f)))

(define rewrite-rules '())
(define macros '())

(define (define-rule o)
  (assert (fun? o))
  (let* ((pat (cadr o))
         (guard (caddr o))
         (body (cadddr o))
         (rule (list pat guard body)))
    (if (macro? o)
        (set! macros (snoc macros rule))
        (set! rewrite-rules (snoc rewrite-rules rule)))
    (if verbose-show-defined-rules
        (begin (display "* ") (shew o)))))

(define (guard-eval env guard)
  (let ((blah (apply-match-env env guard)))
    (let ((nf (normal-form blah)))
      (cond
       ((equal? nf ''true) #t)
       ((equal? nf ''false) #f)
       (#t (err 'guard-eval 'guard-not-a-boolean blah '=> nf))))))

(define (try-to-rewrite-funlikes funlikes e)
  (find-first-maybe
   (lambda (rewrite)
     (let ((pat (car rewrite))
           (guard (cadr rewrite))
           (body (caddr rewrite)))
       ((maybe-compose
         (lambda () (match pat e))
         (lambda (env) (if (guard-eval env (cadr guard)) (just env) fail))
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
  (display "+ ")
  (hshew e)
  (let ((nf (normal-form e)))
    (hshew nf)))

(define (exec-top-level-form o)
  (if (fun? o)
      (define-rule o)
      (exec-exp o)))

(define (run-file-src forms)
  (map exec-top-level-form (map preprocess forms)))

;(tracefun orthogonalize orthogonalize-exp orthogonalize-pat orthogonalize-list orthogonalize-list-cdr)
;(tracefun is-quote? pat-ok?)
;(tracefun get-primitive try-to-rewrite-primitives)
;(tracefun maybe-compose maybe-try)
;(tracefun try-to-rewrite-primitives try-to-rewrite try-to-rewrite-macros try-to-rewrite-funs)
;(tracefun try-to-rewrite-funlikes)
;(tracefun normal-form normal-form-kids guard-eval)
;(tracefun match apply-match-env)
;(tracefun preprocess un-preprocess auto-quote-ctor un-auto-quote-ctor)
;(tracefun add-missing-guard)
;(tracefun guard? fun? fun-with-guard? fun-without-guard?)
