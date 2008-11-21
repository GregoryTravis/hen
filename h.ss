(load "lib.ss")

(define (lam? e) (and (pair? e) (eq? (car e) '/.)))
(define var-of cadr)
(define body-of caddr)

;; (define (evl e . args)
;;   (if (null? args)
;;       (evl e '())
;;       (cond
;;        (lam? e) (cond
;;                  ((lam? (body-of e)

(define (bracket e v)
  (cond
   ((and (lam? e) (eq? (var-of e) v)) (err 'alpha!))

;   ((lam? e) `(/. ,(var-of e) ,(bracket (body-of e) v)))
   ((lam? e) (bracket (bracket (body-of e) (var-of e)) v))

   ((and (pair? e) (> (length e) 2))
    (let ((curried (cons (list (car e) (cadr e)) (cddr e))))
      (bracket curried v)))
   ((and (pair? e) (= (length e) 2)) `(S ,(bracket (car e) v) ,(bracket (cadr e) v)))
   ((eq? e v) `I)
   ((symbol? e) `(K ,e))
   (#t (err 'bracket e v))))

(define (eval-combinator e)
  (cond
   ((symbol? e) e)
   ((eq? (car e) 'S)
    (if (< (length e) 4)
        e
        (let ((f (cadr e))
              (g (caddr e))
              (x (cadddr e)))
          `((,f ,x) (,g ,x)))))
   ((pair? e)
    (eval-combinator (map eval-combinator e)))
   (#t (err 'eval-combinator e))))

;(tracefun bracket)
(tracefun eval-combinator)

(shew
 (eval-combinator '(S r s x))
 (bracket 'y 'x)
 (bracket 'x 'x)
 (bracket '(r x) 'x)
 (bracket '(x r) 'x)
 (bracket '(/. y (r y)) 'x)
)
