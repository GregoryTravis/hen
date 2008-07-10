(load "lib.ss")

(define (top-rw pat t body)
  (rw pat t body body))

(define (subst var x body-template body)
  (cond
   ((eq? 'fail body) 'fail)
   ((literal? body) body)
   ((and (symbol? body-template)
         (eq? var body-template))
    x)
   ((pair? body-template)
    (cons (subst var x (car body-template) (car body))
          (subst var x (cdr body-template) (cdr body))))
   ((symbol? body) body)
   (#t (err 'subst var x body-template body))))

(define (rw pat t body-template body)
  (cond
   ((and (literal? pat))
    (if (equal? pat t)
        body
        'fail))
   ((and (pair? pat))
    (if (pair? t)
        (rw (cdr pat) (cdr t) body-template (rw (car pat) (car t) body-template body))
        'fail))
   ((and (symbol? pat))
    (subst pat t body-template body))
   (#t (err 'rw pat t body-template body))))

(define (try-rws e rws)
  (try-rws-1 e e rws))

(define (try-rws-1 orig e rws)
  (if (null? rws)
      orig
      (let* ((rule (car rws))
             (pat (cadr rule))
             (body (caddr rule))
             (result (top-rw pat e body)))
        (if (eq? 'fail result)
            (try-rws-1 orig e (cdr rws))
            result))))

(define (gather-rws src) (grep fun? src))
(define (gather-exps src) (grep (fnot fun?) src))

(define (run-src src)
  (let ((rws (gather-rws src))
        (exps (gather-exps src)))
    (map (lambda (e) (shew (try-rws e rws)))
         exps)))

;(tracefun try-rws)
(tracefun subst rw top-rw)

(define (go)
  (run-src (read-objects "src.ss")))

(go)
