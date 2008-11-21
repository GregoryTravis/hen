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

(define (bup e)
  (mtch
   e

   (/. x (/. y b)) (bup `(/. ,x ,(bup `(/. ,y ,b))))

   (/. x (a b)) `((S ,(bup `(/. ,x ,a))) ,(bup `(/. ,x ,b)))

   ('/. x y) (if (eq? x y) `I `(K ,y))

   (a b) (list (bup a) (bup b))

   x (if (symbol? x) x (err 'bup e))

   ))

(define (oik e)
  (mtch
   e

   ;((('S f) g) x) `(,(oik `(,(oik f) ,(oik x))) ,(oik `(,(oik g) ,(oik x))))
   ((('S f) g) x) `((,f ,x) (,g ,x))

   ;(('K x) y) (oik x)
   (('K x) y) x

   ;('I x) (oik x)
   ('I x) x

   (a b) (list (oik a) (oik b))

   x x

   ))

(define (oiks e)
  (let ((ee (oik e)))
    (if (equal? e ee)
        e
        (oiks ee))))

;(tracefun oik oiks)

(map
 (lambda (x)
   (shew x)
   (shew (bup x))
   (shew (oiks (bup x))))
 (list
;;   'a
;;   '(/. r r)
;;   '((/. r r) a)
;;   '(/. r s)
;;   '((/. r s) a)
;;   '(/. r (s t))
;;   '((/. r (s t)) a)
;;   '(/. r (/. s s))
;;   '(/. r (/. s r))
;;   '((/. p (p x)) (/. y y))

  '((/. p ((p a) b)) (/. s (/. t ((qq s) t))))

;;  (eval-combinator '(S r s x))
;;  (bracket 'y 'x)
;;  (bracket 'x 'x)
;;  (bracket '(r x) 'x)
;;  (bracket '(x r) 'x)
;;  (bracket '(/. y (r y)) 'x)
))
