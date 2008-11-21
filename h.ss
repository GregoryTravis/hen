(load "lib.ss")

(define (run-file filename)
  (map evl-top (read-objects filename)))

(define (evl-top e)
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

   x (if (atom? x) x (err 'ski e))

   ))

(define (evl-step e)
  (mtch
   e

   ;((('S f) g) x) `(,(evl-step `(,(evl-step f) ,(evl-step x))) ,(evl-step `(,(evl-step g) ,(evl-step x))))
   ((('S f) g) x) `((,f ,x) (,g ,x))

   ;(('K x) y) (evl-step x)
   (('K x) y) x

   ;('I x) (evl-step x)
   ('I x) x

   (a b) (list (evl-step a) (evl-step b))

   x x

   ))

(define (evl e)
  (let ((ee (evl-step e)))
    (if (equal? e ee)
        e
        (evl ee))))
