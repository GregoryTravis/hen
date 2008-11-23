(load "lib.ss")

(define (run-file filename)
  (map evl-top (read-objects filename)))

(define (check-file filename)
  (map evl-check (read-objects filename)))

(define (evl-check e)
  (let* ((ce (ski e))
         (ee (->/. ce))
         (cee (ski ee))
         (cev (evl ce))
         (ceev (evl cee)))
;;     (shew '---)
;;     (shew e)
;;     (shew ce)
;;     (shew ee)
;;     (shew cee)
;;     (shew (evl ce))
;;     (shew (evl cee))
    (assert (equal? cev ceev) cev ceev)))

(define (evl-top e)
  ;(evl-check e)
  (display "+ ")
  (lshew e)
  (display "\n")
  (let ((ce (ski e)))
    (display "- ")
    (lshew ce)
    (display "\n")
    (display "=> ")
    (let ((ee (evl ce)))
      (lshew ee)
      (display "\n\n"))))

(define (ski e)
  (mtch
   e

   (/. x (/. y b)) (ski `(/. ,x ,(ski `(/. ,y ,b))))

   (/. x (a b)) `((S ,(ski `(/. ,x ,a))) ,(ski `(/. ,x ,b)))

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

(define (evl-step s)
  (mtch
   s

   ('S (d0 f) (d1 g) (d2 x) . tail)
   `(((,f ,x) (,g ,x)) . ,tail)

   ('K (d0 x) (d1 y) . tail)
   `(,x . ,tail)

   ('I (d0 x) . tail)
   `(,x . ,tail)

   ((a . d) . dd)
   `(,a (,a . ,d) . ,dd)

;;    '(S) s
;;    '(K) s
;;    '(I) s

   x x

   ))

;; (define (data? o)
;;   (or (and (symbol? o) (not (member? o '(S K I))))
;;       (number? o)))
;; (define (done? s) (and (= (length s) 1) (data? (car s))))

(define (evl s)
  (let ((ss (evl-step s)))
    (if (equal? s ss)
        (car s)
        (evl ss))))
;;     (if (done? ss)
;;         (car ss)
;;         (evl ss))))

(define (evl-top e)
  (display "+ ")
  (lshew e)
  (display "\n")
  (let ((ce (ski e)))
    (display "- ")
    (lshew ce)
    (display "\n")
    (let ((ee (evl (list ce))))
      (display "=> ")
      (lshew ee)
      (display "\n\n"))))

;(tracefun evl-top evl evl-step)
