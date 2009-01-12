(load "lib.ss")

(define (rewrite-body body env)
  (cond
   ((null? body) body)
   ((ctor? body) body)
   ((symbol? body) (if (lookup-exists? body env) (lookup body env) body))
   ((pair? body) (cons (rewrite-body (car body) env) (rewrite-body (cdr body) env)))
   (#t (err 'rewrite-body body env))))

(define (try-match rules pat e)
  (cond
   ((ctor? pat) (if (eq? (drive rules e) pat) '() '(no-match)))
   ((pair? pat)
    (if (and (pair? e) (eq? (car e) (car pat)) (= (length e) (length pat)))
        (apply append (map (lambda (e p) (try-match rules p (if (cton? p) (drive rules e) e))) (cdr e) (cdr pat)))
        '(no-match)))
  ((symbol? pat) (list (cons pat e)))
  (#t (err 'try-match e pat))))

(define (try-rule rules rule e)
  (let ((pat (car rule))
        (body (cadr rule)))
    (let ((env (try-match rules pat e)))
      (if (all? (map (fnot (feq? 'no-match)) env))
          (rewrite-body body env)
          'no-match))))

(define (try-rewrite all-rules rules e)
  (if (null? rules)
      e
      (let ((ee (try-rule all-rules (car rules) e)))
        (if (eq? ee 'no-match)
            (try-rewrite all-rules (cdr rules) e)
            ee))))

(define (primcall? pc)
  (mtch pc ('Primcall f args) #t x #f))

(define (eval-primcall pc)
  (mtch pc
        ('Primcall f args)
        (apply (eval f) args)))

(define (step rules e)
  (cond
   ((primcall? e) (eval-primcall e))
   ((or (ctor? e) (cton? e)) e)
   ((pair? e) (try-rewrite rules rules e))
   (#t (err 'step e))))

(define (drive rules e)
  (if (or (cton? e) (ctor? e))
      e
      (let ((ee (step rules e)))
        (if (equal? e ee)
            e
            (drive rules ee)))))

(define (evl-fully rules e)
  (let ((ee (drive rules e)))
    (cond
     ((ctor? ee) ee)
     ((cton? ee) (map ($ evl-fully rules) ee))
     (#t (err 'wha e ee)))))

(define (run-file filename)
  (let* ((forms (append
                 (read-objects "overture.ss")
                 (read-objects filename)))
         (blah (divide-by-pred (lambda (e) (and (pair? e) (eq? (car e) 'fun))) forms))
         (funs (car blah))
         (tlfs (cdr blah))
         (rules (map (lambda (fun) (list (cadr fun) (caddr fun))) funs)))
    (map (lambda (e)
           (display "+ ") (lshewn e)
           (let ((ee (evl-fully rules e)))
             (display "=> ") (lshewn ee)))
         tlfs)))

;(tracefun rewrite-body)
;(tracefun evl-fully drive step try-rewrite try-rule try-match)
;(tracefun drive)
