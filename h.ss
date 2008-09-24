(load "lib.ss")
(load "sb.ss")
(load "primitives.ss")
(load "assemble.ss")

(define global-env '())
(define (create-global-env globals)
  (set! global-env (var-declarations->env globals)))
(define (var-declarations->env globals)
  (map global-var->binding globals))
;; (define (env-exists? e env)
;;   (not (eq? #f (assoc e env))))
;; (define (env-lookup e env)
;;   (assert (env-exists? e env) e env)
;;   (cdr (assoc e env)))
(define env-exists? lookup-exists?)
(define env-lookup lookup)
(define (global-exists? e) (env-exists? e global-env))
(define (global-lookup e) (env-lookup e global-env))

(define (mitch pat e)
  (cond
   ((var? pat) (list (cons (var-name pat) e)))
   ((app? pat)
    (if (and (app? e) (= (length pat) (length e)))
        (let ((submatches (zip mitch pat e)))
          (if (any? (map (lambda (f) (eq? 'fail f)) submatches))
              'fail
              (apply append submatches)))
        'fail))
   ((atom? pat) (if (equal? pat e)
                    '()
                    'fail))
   (#t (err))))

(define (rewrite body bindings)
  (cond
   ((var? body) (env-lookup (var-name body) bindings))
   ((app? body) (map (lambda (e) (rewrite e bindings))
                     body))
   ((atom? body) body)
   ((is-quote? body) body)
   (#t (err 'rewrite body))))

(define (try-rw e rw rws)
  (assert (fun? rw) rw)
  (let* ((pat (cadr rw))
         (guard (caddr rw))
         (body (cadddr rw))
         (bindings (mitch pat e)))
    (if (eq? 'fail bindings)
        'fail
        (if (equal? (normalize (rewrite guard bindings) rws) 'true)
            (rewrite body bindings)
            'fail))))

(define (try-rws e rws all-rws)
  (if (null? rws)
      'fail
      (let ((poo (try-rw e (car rws) all-rws)))
        (if (eq? 'fail poo)
            (try-rws e (cdr rws) all-rws)
            poo))))

(define (normalize-conditional c rws)
  (assert (conditional? c))
  (let ((if-part (cadr c))
        (then-part (caddr c))
        (else-part (cadddr c)))
    (let ((b (normalize if-part rws)))
      (cond
       ((eq? 'true b) (normalize then-part rws))
       ((eq? 'false b) (normalize else-part rws))
       (#t (err 'conditional-exp-not-boolean c b))))))

(define (normalize-primitive e rws)
  (do-primitive-call (cons (car e)
                           (map (lambda (e) (normalize e rws)) (cdr e)))))

(define (normalize-step e rws)
  (cond
   ((and (symbol? e) (global-exists? e)) (global-lookup e))
   ((conditional? e) (normalize-conditional e rws))
   ((primitive-call? e) (normalize-primitive (cadr e) rws))
   (#t (let ((e (if (app? e)
                    (map (lambda (e) (normalize e rws)) e)
                    e)))
         (let ((s (try-rws e rws rws)))
           (if (eq? 'fail s)
               e
               s))))))

(define (normalize e rws)
  (let ((ee (normalize-step e rws)))
    (if (or (equal? e ee) (eq? 'fail ee))
        (normalized-done e)
        (normalize ee rws))))

(define (normalized? e)
  (or (literal? e)
      (symbol? e)
      (cton? e)))

(define (normalized-done e)
  (if (not (normalized? e))
      (err 'bleargh e)
      e))

(define (evl e rws)
  (display "+ ")
  (sb e)
  (sb (normalize e rws))
  (display "\n"))

;(tracefun evl normalize normalize-step try-rws try-rw)
;(tracefun try-rw mitch rewrite)
;(tracefun normalize normalize-conditional)
;(tracefun normalize)

(define (preprocess src)
  (set! src (primitivize src))
  src)

(define (unpreprocess src)
  (set! src (unprimitivize src))
  src)

(define (mark-vars pat)
  (cond
   ((is-quote? pat)
    ;; HEY extend to quoted non-scalars
    (begin
      (assert (symbol? (quote-quoted pat)))
      (quote-quoted pat)))
   ((var? pat) (err))
   ((symbol? pat) (make-var pat))
   ((list? pat)
    (cond
     ((null? pat) '())
     ((is-quote? (car pat)) (err))
     ((or (symbol? (car pat)) (is-var? (car pat)))
      (cons (car pat) (map mark-vars (cdr pat))))
     ((list? (car pat))
      (map mark-vars pat))
     (#t (err))))
   ((literal? pat) pat)
   (#t (err 'mark-vars pat))))

(define (gather-vars pat)
  (cond
   ((is-quote? pat) '())
   ((symbol? pat) '())
   ((var? pat) (list (var-name pat)))
   ((list? pat) (map-append gather-vars pat))
   ((literal? pat) '())
   (#t (err 'gather-vars pat))))

(define (mark-these-vars pat these-vars)
  (cond
   ((null? pat) '())
   ((is-quote? pat) pat)
   ((member? pat these-vars) (make-var pat))
   ((var? pat) (err 'var pat))
   ((list? pat) (map (lambda (pat) (mark-these-vars pat these-vars))
                     pat))
   ((atom? pat) pat)
   (#t (err 'otherwise pat))))

(define sg (symbol-generator-generator))

(define (remove-nonlinearity pat)
  (let* ((tpat (nl-tag-vars pat))
         (vars (group-by car (nl-gather-tagged-vars tpat)))
         (nls (grep (lambda (var) (> (length var) 2)) vars))
         (nlvars (map car nls))
         (repat (nl-reassign-vars nlvars tpat))
         (comps (nl-build-comparisons nls)))
    (cons repat comps)))

(define (nl-tag-vars pat)
  (cond
   ((app? pat) (map nl-tag-vars pat))
   ((or (literal? pat) (symbol? pat)) pat)
   ((var? pat) (cons pat (list 'unquote (sg))))
   (#t (err 'nl-tag-vars pat))))

(define (nl-gather-tagged-vars pat)
  (cond
   ((and (pair? pat) (var? (car pat))) (list pat))
   ((app? pat) (map-append nl-gather-tagged-vars pat))
   ((or (literal? pat) (symbol? pat)) '())
   (#t (err 'nl-gather-tagged-vars pat))))

(define (nl-reassign-vars nlvars pat)
  (cond
   ((and (pair? pat) (var? (car pat)))
    (if (member? (car pat) nlvars)
        (cdr pat)
        (car pat)))
   ((app? pat) (map (lambda (pat) (nl-reassign-vars nlvars pat)) pat))
   ((or (literal? pat) (symbol? pat)) pat)
   (#t (err 'nl-reassign-vars pat))))

(define (nl-build-comparisons vars)
  (if (null? vars)
      'true
      (cons 'and
            (map-append (lambda (varlist)
                          (let ((renames (cdr varlist)))
                            (map (lambda (rename)
                                   `(== ,(cdar renames) ,(cdr rename)))
                                 (cdr renames))))
                        vars))))

;; Har de har, the default guards are (and true true), which would
;; work except that the definition of 'and' has that guard too, har de
;; har har.
(define (simplify-guard pat)
  (cond
   ((and (pair? pat)
         (eq? 'and (car pat))
         (pair? (cdr pat))
         (eq? 'true (cadr pat))
         (pair? (cddr pat))
         (null? (cdddr pat)))
    (simplify-guard (caddr pat)))
   ((and (pair? pat)
         (eq? 'and (car pat))
         (pair? (cdr pat))
         (null? (cddr pat)))
    (cadr pat))
   (#t pat)))

;; (define (add-guard e)
;;   (assert (fun-without-guard? e))
;;   `(fun ,(cadr e) (? true) ,(caddr e)))

;; Add guard if rule doesn't have one, and strip the guard syntax (? _)
(define (fun-standardize-guard e)
  (cond
   ((fun-without-guard-syntax? e) `(fun ,(cadr e) true ,(caddr e)))
   ((fun-with-guard-syntax? e) `(fun ,(cadr e) ,(cadr (caddr e)) ,(caddr (cdr e))))
   (#t (err 'fun-standardize-guard e))))

;      (add-guard r)
;      r))
;;   (set! r (if (fun-without-guard? r) (add-guard r) r))
;;   (assert (fun-with-guard? r))
;;   `(fun ,(cadr r) ,(cadr (caddr r)) ,(cadddr r)))

(define (preprocess-rule rw)
  (assert (fun-src-syntax? rw))
  (set! rw (fun-standardize-guard rw))
  (let* ((pat (cadr rw))
         (guard (caddr rw))
         (body (cadddr rw))
         (ppat (mark-vars pat))
         (vars (gather-vars ppat))
         (pguard (mark-these-vars guard vars))

         ;; nonlinearity
         (stuff (remove-nonlinearity ppat))
         (ppat (car stuff))
         (pguard `(and ,pguard ,(cdr stuff)))
         (pguard (simplify-guard pguard))

         (pbody (mark-these-vars body vars)))
    `(fun ,ppat ,pguard ,pbody)))

;(tracefun preprocess-rule mark-these-vars mark-vars gather-vars)
;(tracefun mark-these-vars)

(define (gather-rws src) (grep fun-src-syntax? src))
(define (gather-global-vars src) (grep global-var? src))
(define (gather-exps src) (grep (fnot (for fun-src-syntax? global-var?)) src))

(define (run src)
  (let* ((src (preprocess src))
         (rws (map preprocess-rule (gather-rws src)))
         (globals (gather-global-vars src))
         (exps (gather-exps src)))
    (create-global-env globals)
    (map (lambda (e) (evl e rws)) exps)))

(define (go)
  (run (load-files (list "src.ss"))))
;(load "tracing.ss")
;(tracefun normalize); try-rw)
