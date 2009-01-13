(load "lib.ss")

(define run-eagerly #t)

(define steps 0)
(define (count-step) (set! steps (+ steps 1)))
(define (report-steps) (shew 'steps steps))

(define (rewrite-body body env)
  (cond
   ((null? body) body)
   ((ctor? body) body)
   ((symbol? body) (if (lookup-exists? body env) (lookup body env) body))
   ((pair? body) (cons (rewrite-body (car body) env) (rewrite-body (cdr body) env)))
   (#t (err 'rewrite-body body env))))

(define (try-match e pat rules)
  (cond
   ((and (symbol? pat) (not (ctor? pat))) (list (cons pat e)))
   ((ctor? pat) (if (eq? (drive e rules) pat) '() '(no-match)))
   ((pair? pat)
    (if (and (pair? e) (eq? (car e) (car pat)) (= (length e) (length pat)))
        (apply append (map (lambda (e p) (try-match (if (cton? p) (drive e rules) e) p rules)) (cdr e) (cdr pat)))
        '(no-match)))
  (#t (err 'try-match e pat))))

(define (try-rule e rule rules)
  (let ((pat (car rule))
        (body (cadr rule)))
    (let ((env (try-match e pat rules)))
      (if (all? (map (fnot (feq? 'no-match)) env))
          (rewrite-body body env)
          'no-match))))

(define (try-rewrite e rules all-rules)
  (if (null? rules)
      e
      (let ((ee (try-rule e (car rules) all-rules)))
        (if (eq? ee 'no-match)
            (try-rewrite e (cdr rules) all-rules)
            ee))))

(define (primcall? pc)
  (mtch pc ('Primcall f args) #t x #f))

(define (eval-primcall pc)
  (mtch pc
        ('Primcall f args)
        (apply (eval f) args)))

(define (eager-maybe e rules)
  (if run-eagerly
      (cons (car e) (map (lambda (e) (evl-fully e rules)) (cdr e)))
      e))

(define (step e rules)
  (count-step)
  (cond
   ((primcall? e) (eval-primcall e))
   ((or (ctor? e) (cton? e)) e)
   ((pair? e) (try-rewrite (eager-maybe e rules) rules rules))
   ((symbol? e) e) ; HEY
   (#t (err 'step e))))

(define (drive e rules)
  (if (or (cton? e) (ctor? e))
      e
      (let ((ee (step e rules)))
        (if (equal? e ee)
            e
            (drive ee rules)))))

(define (evl-fully e rules)
  (let ((ee (drive e rules)))
    (cond
     ((ctor? ee) ee)
     ((cton? ee) (map (lambda (e) (evl-fully e rules)) ee))
     ((symbol? ee) ee) ; HEY
     (#t (err 'wha e ee)))))

;; (define gip 0)
;; (wrap-before-and-after evl-fully
;;                        (lambda () (set! gip (+ gip 1)))
;;                        (lambda () (set! gip (- gip 1))))
;; (wrap-args-and-result evl-fully (lambda (e rules result)
;;                                   (if (not (equal? e result))
;;                                       (begin
;;                                         (display (make-string-string gip "-"))
;;                                         (lshew e)
;;                                         (display " -> ")
;;                                         (lshew result)
;;                                         (display "\n"))
;;                                       '())))

(define (run-file filename)
  (let* ((forms (append
                 (read-objects "overture.ss")
                 (read-objects filename)))
         (forms (unsugar forms))
         (blah (divide-by-pred (lambda (e) (and (pair? e) (eq? (car e) 'fun))) forms))
         (funs (car blah))
         (tlfs (cdr blah))
         (rules (map (lambda (fun) (list (cadr fun) (caddr fun))) funs)))
    (map (lambda (e)
           (display "+ ") (lshewn (sugar e))
           (let ((ee (evl-fully e rules)))
             (display "=> ") (lshewn (sugar ee))))
         tlfs)))

(define (integer->bits i) (reverse (integer->bits-1 i)))
(define (integer->bits-1 i)
  (if (= i 0)
      '()
      (cons (if (= (bitwise-and i 1) 1) 'True 'False) (integer->bits-1 (arithmetic-shift i -1)))))

(define (integer->ctors i)
  `(Integer ,(consify (integer->bits i))))

(define (ctors->integer i) (ctors->integer-1 i 0))
(define (ctors->integer-1 i a)
  (mtch i
        'Nil a
        ('Cons 'True j) (ctors->integer-1 j (+ (* a 2) 1))
        ('Cons 'False j) (ctors->integer-1 j (* a 2))))

(define (unsugar e)
  (cond
   ((pair? e) (cons (unsugar (car e)) (unsugar (cdr e))))
   ((integer? e) (integer->ctors e))
   ((or (symbol? e) (null? e)) e)
   (#t (err 'unsugar e))))

(define (sugar e)
  (mtch e
        ('Integer i) (ctors->integer i)
        (a . b) (cons (sugar a) (sugar b))
        x x))

;(tracefun rewrite-body)
;(tracefun evl-fully drive step try-rewrite try-rule try-match)
;(tracefun drive)
;(tracefun unsugar)
