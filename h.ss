(load "lib.ss")

(define (run-file filename)
  (map
   (lambda (e)
     (mtch
      e

      ('fun (name . args) . body)
      (define-fun name args body)

      x
      (evl-top x)))
   (read-objects filename)))

(define global-env '())
(define (define-fun name args body)
  (assert (= (length args) 1))
  (set! global-env
        (cons (cons name (ski `(/. ,(car args) . ,body))) global-env)))

(define (evl-check e)
  (let* ((ce (ski e))
         (ee (->/. ce))
         (cee (ski ee))
         (cev (evl-fully ce))
         (ceev (evl-fully cee)))
;;     (shew '---)
;;     (shew e)
;;     (shew ce)
;;     (shew ee)
;;     (shew cee)
;;     (shew cev)
;;     (shew ceev)
    (assert (equal? cev ceev) e ce ee cee cev ceev)))

(define (ski e)
  (mtch
   e

   ('/. x (/. y b)) (ski `(/. ,x ,(ski `(/. ,y ,b))))

   ('/. x (a b)) `((S ,(ski `(/. ,x ,a))) ,(ski `(/. ,x ,b)))

   ('/. x y) (if (eq? x y) `I `(K ,y))

   (a b) (list (ski a) (ski b))

   x (if (or (symbol? x) (number? x) (string? x)) x (err 'ski e))

   ))

(define sg (symbol-generator-generator))

(define (->/. e)
  (let ((v0 (sg))
        (v1 (sg)))
    (mtch
     e

     (('S a) b) `(/. ,v0 ((,(->/. a) ,v0) (,(->/. b) ,v0)))

     ((('S a) b) c) `((,(->/. a) ,(->/. c)) (,(->/. b) ,(->/. c)))

     ('K a) `(/. ,v0 ,(->/. a))

     (('K a) b) (->/. a)

     'I `(/. ,v0 ,v0)

     ('I a) (->/. a)

     (a b) `(,(->/. a) ,(->/. b))

     x x

     )))

;; (define (evl-step s)
;;   (mtch
;;    s

;;    ('S (d0 f) (d1 g) (d2 x) . tail)
;;    `(((,f ,x) (,g ,x)) . ,tail)

;;    ('K (d0 x) (d1 y) . tail)
;;    `(,x . ,tail)

;;    ('I (d0 x) . tail)
;;    `(,x . ,tail)

;;    ((a . d) . dd)
;;    `(,a (,a . ,d) . ,dd)

;;    ('S f g) s
;;    ('S f) s
;;    ('S) s
;;    ('K x) s
;;    ('K) s
;;    ('I) s

;;    ('+ ('+ a) (('+ aa) b)) (list (+ (evl0 (list a)) (evl0 (list b))))
;;    ('- ('- a) (('- aa) b)) (list (- (evl0 (list a)) (evl0 (list b))))
;;    ('* ('* a) (('* aa) b)) (list (* (evl0 (list a)) (evl0 (list b))))
;;    ('== ('== a) (('== aa) b)) (list (if (smart= (evl0 (list a)) (evl0 (list b))) 'True 'False))
;;    ('if ('if b) (('if bb) t) ((('if bb) t) e)) (list (mtch (evl0 (list b)) 'True t 'False e))

;;    (a . rest) (cond
;;                ((and (ctor? a) (null? rest)) s)
;;                ((lookup-exists? a global-env) (cons (lookup a global-env) rest))
;;                ((symbol? a) (err 'what-is (car s) s))
;;                ((number? a) s)
;;                (#t (err 'bad-form s)))

;;    ))

;; ;; (define (data? o)
;; ;;   (or (and (symbol? o) (not (member? o '(S K I))))
;; ;;       (number? o)))
;; ;; (define (done? s) (and (= (length s) 1) (data? (car s))))

;; (define (done-stack s)
;;   (if (= (length s) 1)
;;       (car s)
;;       (done-stack (cons (cons (car s) (cdadr s)) (cddr s)))))
;;   ;(cons (car s) (map cadr (cdr s))))

;; (define (evl0 s) (evl s))

;; (define (evl s)
;;   (let ((ss (evl-step s)))
;;     (if (equal? s ss)
;;         (done-stack s)
;;         (evl ss))))
;; ;;     (if (done? ss)
;; ;;         (car ss)
;; ;;         (evl ss))))

;; (define (evl-fully e)
;;   (let ((e (evl0 (list e))))
;;     (mtch
;;      e

;; ;     ('S a) (evl0 (list e))
;; ;     ('K a) (evl0 (list e))
;; ;     ('I a) (evl0 (list e))

;;      (a b) (list (evl-fully (car e))
;;                  (evl-fully (cadr e)))

;;      x (if (or (number? x) (symbol? x)) x (err 'evl-fully e)))))

(define (evl-step e)
  (mtch
   e

   ((('S f) g) x)
   `((,f ,x) (,g ,x))

   (('K x) y)
   x

   ('I x)
   x

   (('S f) g) e
   ('S f) e
   'S e
   ('K x) e
   'K e
   'I e
   ('+ x) e
   '+ e
   ('- x) e
   '- e
   ('* x) e
   '* e
   ('== x) e
   '== e
   (('if b) t) e
   ('if b) e
   'if e

   (('+ a) b) (+ (evl-fully a) (evl-fully b))
   (('- a) b) (- (evl-fully a) (evl-fully b))
   (('* a) b) (* (evl-fully a) (evl-fully b))
   (('== a) b) (if (smart= (evl-fully a) (evl-fully b)) 'True 'False)
   ((('if b) t) e) (mtch (evl-fully b) 'True t 'False e)
   
   (a b) (evl-step (list (evl-minimally a) b))

   x (cond
      ((done-fully? x) x)
      ((and (symbol? x) (lookup-exists? x global-env)) (lookup x global-env))
      (#t (err 'evl-step e)))
   ))

(define (done-minimally? e)
  (mtch
   e

   (('S f) g) #t
   ('S f) #t
   'S #t
   ('K x) #t
   'K #t
   'I #t

   (a . b) #f

   x (done-fully? e)

   ))

(define (evl-minimally e)
  (if (done-minimally? e)
      e
      (evl-minimally (evl-step e))))

(define (done-fully? e)
  (or (number? e) (ctor? e)))

(define (evl-fully e)
  (if (done-fully? e)
      e
      (let ((ee (evl-minimally e)))
        (mtch ee
              (a b) (evl-fully (list (evl-fully a) (evl-fully b)))
              x (if (done-fully? x) x (err 'evl-fully e ee))
              ))))

(define (evl-top e)
  ;(evl-check e)
  (display "+ ")
  (lshew e)
  (display "\n")
  (let ((ce (ski e)))
    (display "- ")
    (lshew ce)
    (display "\n")
    (let ((ee (evl-fully ce)))
      (display "=> ")
      (lshew ee)
      (display "\n\n")
      ee)))

;(tracefun evl-top evl-fully evl-minimally evl-step)
;(tracefun ski)
