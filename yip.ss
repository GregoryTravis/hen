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
   ((ctor? pat) (eq? e pat))
   ((pair? pat)
    (if (and (pair? pat) (eq? (car e) (car pat)) (= (length e) (length pat)))
        (map-append (lambda (e p) (try-match rules p (if (cton? p) (drive rules e) e))) (cdr e) (cdr pat))
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

(define (step rules e)
  (cond
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
     ((cton? ee) (map (lambda (e) (evl-fully rules e)) ee))
     (#t (err 'wha ee)))))

;(tracefun drive step try-rewrite try-rule try-match rewrite-body)

;; (define joot
;;   '(
;;     ((foo a) (Bar a))
;;     ((foo a b) (Boot (foo a) (foo b)))
;;     ((zek (Bar (Bar x))) (Shy x))
;;     ((foo (Bar a) b) (Book b a))
;;     ((het a b) (Yip a b))
;;     ((hat (Yip a b)) (Yap b a))
;;     ((hick a) (Vot a a))
;;     ))

;; (define terms
;;   '(
;;     A
;;     (foo Ten)
;;     (foo (Bar Ten) Seventy)
;;     (foo (foo Ten))
;;     (zek (foo (foo Ten)))
;;     (hick (foo (foo Yah)))
;;     (Joo (foo (Bar Ten) Seventy))
;;     (hat (het Ten Twenty))
;;     (foo One Two)
;;     ))

;; (shew (map (lambda (e) (drive joot e)) terms))
;; (shew (map (lambda (e) (evl-fully joot e)) terms))
