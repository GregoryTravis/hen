(load "lib.ss")

;; (define (try-match e pat is-head)
;;   (cond
;;    ((and (null? e) (null? pat)) '())
;;    ((ctor? pat) (if (eq? e pat) '() '(no-match)))
;; ;(and is-head (symbol? pat))
;;    ((symbol? pat) (list (cons pat e)))
;;    ((and (pair? pat) (pair? e))
;;     (if is-head
;;         (if (eq? (car pat) (car e))
;;             (try-match (cdr e) (cdr pat) #f)
;;             '(no-match))
;;         (append (try-match (car e) (car pat) #t) (try-match (cdr e) (cdr pat) #f))))
;;    (#t (err 'try-match e pat is-head))))

(define (rewrite-body body env)
  (cond
   ((null? body) body)
   ((ctor? body) body)
   ((symbol? body) (if (lookup-exists? body env) (lookup body env) body))
   ((pair? body) (cons (rewrite-body (car body) env) (rewrite-body (cdr body) env)))
   (#t (err 'rewrite-body body env))))

(define (try-match e pat)
  (cond
   ((ctor? pat) (eq? e pat))
   ((pair? pat)
    (if (and (pair? pat) (eq? (car e) (car pat)) (= (length e) (length pat)))
        (map-append (lambda (e p) (try-match (if (cton? p) (drive e) e) p)) (cdr e) (cdr pat))
        '(no-match)))
  ((symbol? pat) (list (cons pat e)))
  (#t (err 'try-match e pat))))

(define (try-rule e rule)
  (let ((pat (car rule))
        (body (cadr rule)))
    (let ((env (try-match e pat)))
      (if (all? (map (fnot (feq? 'no-match)) env))
          (rewrite-body body env)
          'no-match))))

(define (try-rewrite e rules)
  (if (null? rules)
      e
      (let ((ee (try-rule e (car rules))))
        (if (eq? ee 'no-match)
            (try-rewrite e (cdr rules))
            ee))))

(define (step e)
  (cond
   ((or (ctor? e) (cton? e)) e)
   ((pair? e) (try-rewrite e rules))
   (#t (err 'step e))))

(define (drive e)
  (if (or (cton? e) (ctor? e))
      e
      (let ((ee (step e)))
        (if (equal? e ee)
            e
            (drive ee)))))

(define (evl-fully e)
  (let ((ee (drive e)))
    (cond
     ((ctor? ee) ee)
     ((cton? ee) (map evl-fully ee))
     (#t (err 'wha ee)))))

;(tracefun drive step try-rewrite try-rule try-match rewrite-body)
