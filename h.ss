(load "lib.ss")

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
  'a
  '(/. r r)
  '((/. r r) a)
  '(/. r s)
  '((/. r s) a)
  '(/. r (s t))
  '((/. r (s t)) a)
  '(/. r (/. s s))
  '(/. r (/. s r))
  '((/. p (p x)) (/. y y))
  '((/. p ((p a) b)) (/. s (/. t ((qq s) t))))
))
