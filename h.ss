(load "lib.ss")

(define (run-file filename)
  (run-src (read-objects filename)))

(define (fun? e)
  (and (pair? e) (eq? (car e) 'fun)))

(define (def? e)
  (and (pair? e) (eq? (car e) 'def)))

(define (fun->def e)
  (mtch e
        ('fun (name arg) body)
        `(def ,name (/. ,arg ,body))

        x x))

(define (preprocess-def e)
  (mtch e
        ('def name e)
        `(def ,name ,(preprocess e))))

(define (run-src forms)
  (let* ((forms (map fun->def forms))
         (defs (map preprocess-def (grep def? forms)))
         (tlfs (map preprocess (grep (fnot def?) forms))))
    (map define-def defs)
    ;(dump-globals)
    (map evl-top tlfs)))

(define (preprocess e)
  (mtch e
        ('/. args body) `(/. ,args ,(preprocess body))
        (a b c . rest) (preprocess `((,a ,b) ,c . ,rest))
        (a b) (list (preprocess a) (preprocess b))
        x x))

(define global-env '())
(define (define-def e)
  (mtch e
        ('def name e)
        (set! global-env
              (cons (cons name (ski e)) global-env))))
(define (dump-globals)
  (shew global-env))

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
;;     (shew cev)
;;     (shew ceev)
    (assert (equal? cev ceev) e ce ee cee cev ceev)))

(define (ski e)
  (mtch
   e

   ;('/. (a . d) body) `(U ,(ski `(/. ,a (/. ,d ,body))))

   ('/. x (/. y b)) (ski `(/. ,x ,(ski `(/. ,y ,b))))

   ('/. x (a b)) `((S ,(ski `(/. ,x ,a))) ,(ski `(/. ,x ,b)))

   ('/. x y) (if (eq? x y) `I `(K ,y))

   (a b) (list (ski a) (ski b))

   (a . b) `((P ,(ski a)) ,(ski b))

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

(define (done? e)
  (or (number? e) (ctor? e)))

(define (evl-step e)
  (mtch
   e

   (('P a) b) e

   ;(('U f) (('P x) y))
   ;`((,f ,x) ,y)

   ((('S f) g) x)
   `((,f ,x) (,g ,x))

   (('K x) y)
   x

   ('I x)
   x

   ;('P a) e
   ;('U f) e
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

   ('pair? p) (let ((p (evl p))) (mtch p (('P a) b) 'True x 'False))
   'pair? e

   (('cons a) b) `((P ,a) ,b)

   ('car (('P a) b)) a
   ('car x) `(car ,(evl x))

   ('cdr (('P a) b)) b
   ('cdr x) `(cdr ,(evl x))

   ('cons x) e
   'cons e
   'car e
   'cdr e

   (('+ a) b) (+ (evl a) (evl b))
   (('- a) b) (- (evl a) (evl b))
   (('* a) b) (* (evl a) (evl b))
   (('== a) b) (if (smart== (evl a) (evl b)) 'True 'False)
   ((('if b) t) e) (mtch (evl b) 'True t 'False e)
   
   (a b) (evl-step (list (evl a) b))

   x (cond
      ((and (symbol? x) (lookup-exists? x global-env)) (lookup x global-env))
      (#t (err 'evl e)))
   ))

(define (evl e)
  (if (done? e)
      e
      (let ((ee (evl-step e)))
        (if (smart== e ee)
            ee
            (evl ee)))))
      ;(evl (evl-step e))))

(define (evl-top e)
  ;(evl-check e)
  (display "+ ")
  (lshew e)
  (display "\n")
  (let ((ce (ski e)))
    (display "- ")
    (lshew ce)
    (display "\n")
    (let ((ee (evl ce)))
      (display "=> ")
      (lshew ee)
      (display "\n\n")
      ee)))

;(tracefun evl evl-step)
;(tracefun ski)
;(tracefun preprocess)
