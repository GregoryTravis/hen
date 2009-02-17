(load "lib.ss")

;; (define forms (read-objects "src.ss"))
;; (define (go) (map evl forms))
(define (go) 1)

;; (define evl shew)

(define rules
  '(
    ((cton Foo ((var a) (const (opaque 20)) (var c)))
     (cton Bar ((var c) (var a))))))

(define targets
  '((cton Foo ((const (opaque 10)) (const (opaque 20)) (const (opaque 30))))
    (cton Foo ((const (opaque 10)) (const (opaque 20)) (const (opaque 300))))
    (cton Foo ((const (opaque 10)) (const (opaque 200)) (const (opaque 30))))
    (cton Foo ((const (opaque 10)) (const (opaque 20))))))

(define (try-rules rules target)
  (if (null? rules)
      #f
      (let ((m (try-rule (car rules) target)))
        (if (eq? m #f)
            (try-rules (cdr rules) target)
            m))))

(define (try-rule rule target)
  (let ((pat (car rule))
        (body (cadr rule)))
    (let ((env (try-pat pat target)))
      (if (pat-match-failed? env)
          #f
          (rewrite-body body env)))))

(define (pat-match-failed? env)
  (any? (map ($ eq? _ #f) env)))

(define (try-pat pat target)
  (let ((type (car pat)))
    (cond
     ((eq? type 'const) (if (equal? pat target) '() '(#f)))
     ((eq? type 'var) (list (cons (cadr pat) target)))
     ((eq? type 'cton)
      (if (and (eq? (car target) 'cton)
               (eq? (cadr pat) (cadr target))
               (eq? (length (caddr pat)) (length (caddr target))))
          (map-append try-pat ;(lambda (p t) (try-pat p t))
                      (caddr pat)
                      (caddr target))
          '(#f)))
     (#t (err 'try-pat type)))))

(define (rewrite-body body env)
  (let ((type (car body)))
    (cond
     ((eq? type 'const) body)
     ((eq? type 'var) (lookup (cadr body) env))
     ((eq? type 'cton)
      `(,(car body) ,(cadr body) . ,(map (lambda (b) (rewrite-body b env))
                                         (caddr body))))
     (#t (err 'rewrite-body type)))))

;(tracefun try-rules try-rule try-pat pat-match-failed?)

(define (strip-opaque e)
  (if (and (pair? e) (eq? (car e) 'opaque))
      (cadr e)
      e))

(define (sugar-consts-pairs-vars e)
  (let ((type (car e)))
    (cond
     ((eq? type 'const)
      (let ((c (cadr e)))
        (if (symbol? c) `(quote, c) (strip-opaque c))))
     ((eq? type 'var) (cadr e))
     ((eq? type 'cton) (cons (cadr e) (map sugar-consts-pairs-vars (caddr e))))
     ((eq? type 'app) (cons (cadr (cadr e)) (map sugar-consts-pairs-vars (caddr e))))
     (#t (err 'sugar-consts-pairs-vars type)))))

(define (unsugar-consts-pairs-vars e)
  (cond
   ((and (pair? e) (ctor? (car e)))
    `(cton ,(car e) ,(map unsugar-consts-pairs-vars (cdr e))))
   ((pair? e)
    `(app ,(unsugar-consts-pairs-vars (car e)) ,(map unsugar-consts-pairs-vars (cdr e))))
   ((symbol? e)
    `(var ,e))
   ((is-quote? e)
    (let ((o (cadr e)))
      (assert (symbol? o))
      `(const ,o)))
   ((or (number? e) (string? e))
    `(const (opaque ,e)))
   (#t (err 'unsugar-consts-pairs-vars e))))

(define (unsugar-head-var-syms->consts e)
  (let ((type (car e)))
    (cond
     ((eq? type 'const) e)
     ((eq? type 'var) e)
     ((eq? type 'app)
      (if (and (pair? (cadr e)) (eq? (caadr e) 'var))
          `(app (const ,(cadadr e)) ,(map unsugar-head-var-syms->consts (caddr e)))
          `(app ,(cadr e) ,(map unsugar-head-var-syms->consts (caddr e)))))
     ((eq? type 'cton) `(cton ,(cadr e) ,(map unsugar-head-var-syms->consts (caddr e))))
     (#t (err 'unsugar-head-var-syms->consts e)))))

(define (sugar-head-const-syms->vars e)
  (let ((type (car e)))
    (cond
     ((eq? type 'const) e)
     ((eq? type 'var) e)
     ((eq? type 'app)
      (if (and (pair? (cadr e)) (eq? (caadr e) 'const))
          `(app (var ,(cadadr e)) ,(map sugar-head-const-syms->vars (caddr e)))
          `(app ,(cadr e) ,(map sugar-head-const-syms->vars (caddr e)))))
     ((eq? type 'cton) `(cton ,(cadr e) ,(map sugar-head-const-syms->vars (caddr e))))
     (#t (err 'sugar-head-const-syms->vars e)))))

(define (sugar e)
  (sugar-consts-pairs-vars (sugar-head-const-syms->vars e)))

(define (unsugar e)
  (unsugar-head-var-syms->consts (unsugar-consts-pairs-vars e)))

(define (sus e)
  (let* ((se (sugar e))
         (use (unsugar se))
         (suse (sugar use)))
    (shew e se use suse)
    (assert (and (equal? e use)
                 (equal? se suse)))
    (shew e se use suse)))

(define (usu e)
  (let* ((ue (unsugar e))
         (sue (sugar ue))
         (usue (unsugar sue)))
    (assert (and (equal? e sue)
                 (equal? ue usue)))
    (shew e ue sue usue)))

(define rules
  '(
    ((Foo a 20 c) (Bar c a))))

(define targets
  `((Foo 10 20 30)
    (Foo 10 20 300)
    (Foo 10 200 30)
    (Foo 10 20)))

(define more-exps
  '((app (const joe) ((cton Hoot ((const (opaque 10))))))
    (app (const joe) ((const (opaque 10))))))

(shew (map ($ try-rules (map (lambda (rule) (map unsugar rule)) rules) _) (map unsugar targets)))
(map usu (apply append rules))
(map usu targets)
(map sus more-exps)
