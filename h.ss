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
  (display "- ")
  (let ((ce (ski e)))
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

(define (evl-step e)
  (mtch
   e

   ((('S f) g) x) `((,f ,x) (,g ,x))

   (('K x) y) x

   ('I x) x

   (('+ a) b) (+ a b)

   (('= a) b) (if (= a b) 'True 'False)

   ((('if b) t) e) (mtch b 'True t 'False e)

   (a b) (list (evl-step a) (evl-step b))

   x x

   ))

(define (evl e)
  (let ((ee (evl-step e)))
    (if (equal? e ee)
        e
        (evl ee))))
