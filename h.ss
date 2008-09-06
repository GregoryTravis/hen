(load "lib.ss")
(load "sb.ss")
(load "primitives.ss")
(load "assemble.ss")

(define global-env '())
(define (create-global-env globals)
  (set! global-env (var-declarations->env globals)))
(define (var-declarations->env globals)
  (map global-var->binding globals))
(define (env-exists? env e)
  (not (eq? #f (assoc e env))))
(define (env-lookup env e)
  (assert (env-exists? env e))
  (cdr (assoc e env)))

(define (try-rw e rw)
  'fail)

(define (try-rws e rws)
  (if (null? rws)
      'fail
      (let ((poo (try-rw e (car rws))))
        (if (eq? 'fail poo)
            (try-rws e (cdr rws))
            poo))))

(define (normalize-step e rws)
  (let ((e (if (app? e)
               (map (lambda (e) (normalize e rws)) e)
               e)))
    (try-rws e rws)))

(define (normalize e rws)
  (let ((ee (normalize-step e rws)))
    (if (or (equal? e ee) (eq? 'fail ee))
        e
        (normalize ee rws))))

(define (evl e rws)
  (display "+ ")
  (shew e)
  (shew (normalize e rws)))

;(tracefun evl normalize normalize-step try-rws try-rw)

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
   (#t (err))))

(define (gather-vars pat)
  (cond
   ((is-quote? pat) '())
   ((symbol? pat) '())
   ((var? pat) (list (var-name pat)))
   ((list? pat) (map-append gather-vars pat))
   (#t (err))))

(define (mark-these-vars pat these-vars)
  (cond
   ((null? pat) '())
   ((is-quote? pat) pat)
   ((member? pat these-vars) (make-var pat))
   ((var? pat) (err 'var pat))
   ((list? pat) (map (lambda (pat) (mark-these-vars pat these-vars))
                     pat))
   ((symbol? pat) pat)
   (#t (err 'otherwise pat))))

(define (preprocess-rule rw)
  (assert (fun? rw))
  (let* ((pat (cadr rw))
         (body (caddr rw))
         (ppat (mark-vars pat))
         (vars (gather-vars ppat))
         (pbody (mark-these-vars body vars)))
    `(fun ,ppat ,pbody)))

;(tracefun preprocess-rule mark-these-vars mark-vars gather-vars)
;(tracefun mark-these-vars)

(define (gather-rws src) (grep fun? src))
(define (gather-global-vars src) (grep global-var? src))
(define (gather-exps src) (grep (fnot (for fun? global-var?)) src))

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
