(load "lib.ss")

;; (define forms (read-objects "src.ss"))
;; (define (go) (map evl forms))
(define (go) 1)

;; (define evl shew)

(define rules
  '(
    ((cton Foo ((var a) (const (opaque 20)) (var c)))
     (cton Bar ((var c) (var a))))))

(define target '(cton Foo ((const (opaque 10)) (const (opaque 20)) (const (opaque 30)))))
;(define target '(cton Foo ((const (opaque 10)) (const (opaque 20)))))

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

(shew (try-rules rules target))
