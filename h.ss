(load "lib.ss")

(define (rule-ok? rule)
  (and (pair? rule)
       (eq? (car rule) 'lambda)
       (proper-list? rule)
       (= (length rule) 3)))

(define (reduce-rule-on-body rule)
  (assert (rule-ok? rule))
  (let ((arg (cadr rule))
        (body (caddr rule)))
    (cond
     ((is-quote? body)
      '???)
     ((symbol? body)
      (if (equal? arg body)
          `(lambda t (lambda s (lambda f (s t))))
          `(lambda t (lambda s (lambda f (f))))))
     ((pair? body)
      `(lambda t
         (lambda s
           (lambda f
             (((,(reduce-rule-on-body `(lambda ,arg ,(car body)))
                t)
               (lambda (tt)
                 ((((,(reduce-rule-on-body `(lambda ,arg ,(cdr body))))
                    t)
                   (lambda (ttt) (cons tt ttt)))
                  f)))
              f)))))
            
;         (consy (,(reduce-rule-on-body `(lambda ,arg ,(car body))) t s f)
;                (,(reduce-rule-on-body `(lambda ,arg ,(cdr body)))) t s f)))
     (#t rule))))

;;      ((is-quote? arg)
;;       `(lambda (t s f)
;;          (if (== t ,(quote-quoted arg))
;;              (s ,body)
;;              (f))))
;;      ((symbol? arg)
;;       (cond
;;        ((is-quote? body)
;;         `(lambda (t s f) (s ,body)))
;;        ((equal? body arg)
;;         `(lambda (t s f) (s t)))
;;        (#t (list rule '???))))
;;      ((pair? arg)
;; ;      (let ((car-reduced (reduce-rule `(lambda ,(car arg) body)))
;; ;            (cdr-reduced (reduce-rule `(lambda ,(cdr arg) body))))
;;       `(lambda (t s f)
;;          (if (pair? t)
;;              (,(reduce-rule `(lambda ,(car arg) 
;;            (,car-reduced ,(car t)
;;                          (lambda (tt) (,cdr-reduced tt s f))
;;                          f))))
;;        (#t rule)))
;; ;     ((symbol? arg)
;; ;      (cond
;; ;      '(lambda (t s f)
;; ;         (s ((lambda arg t)
;;      (#t rule))))

;(define rule '(lambda 'a 10))
;(shew rule (reduce-rule rule))

(define rules
  '(
    ;(lambda 'a 10)
    (lambda x x)
    (lambda x (x . x))
    ;(lambda x 'jones)
    ;(lambda (a . b) 50)
    ;(lambda (x y) (cons y x))
    ))
;; (map (lambda (rule) (shew rule (reduce-rule-on-body rule)))
;;      rules)

;(define (subst

(define (lambda? f)
  (and (= 3 (length f))
       (eq? 'lambda (car f))
       (symbol? (cadr f))))

(define (lc-apply f a)
  (assert (lambda? f))
  (subst (caddr f) (cadr f) a))

(define (lc-reduce e)
  (cond
   ((app? e) (lc-apply (lc-reduce (car e)) (lc-reduce (cadr e))))
   ((lambda? e) e)
   (#t (err 'lc-reduce e))))

;;    ((= 2 (length e))
;;     (lc-apply (car e) (cadr e)))
;;    (#t (err 'lc-reduce e))))

;(tracefun lc-reduce lc-apply)

(define rule '(lambda x x))
(shew rule)
(set! rule (reduce-rule-on-body rule))
(shew rule)
(define input 10)
(shew input)
(define e `(((,rule ,input) (lambda t (success t))) (lambda (t) (fail))))
(shew e)
;(lc-reduce e)
