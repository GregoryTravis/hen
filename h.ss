(load "lib.ss")

(define (run-file filename)
  (run-src (append
            (read-objects "overture.ss")
            (read-objects filename))))

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
        ('/./. . lams) (process-/./. lams)
        ('/. args body) (preprocess `(/./. ,e))
        (a b) (list (preprocess a) (preprocess b))
        (a b c . rest) (err 'preprocess-list-3 e)
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
    ;; (begin (shew '---) (shew e) (shew ce) (shew ee) (shew cee) (shew cev) (shew ceev))
    (assert (equal? cev ceev) e ce ee cee cev ceev)))

(define (process-/./. lams)
  (if (null? lams)
      `(/. x FAIL)
      (process-/. (car lams) (process-/./. (cdr lams)))))

(define (process-/. lam failure)
  (let ((v (sg)))
    (mtch
     lam

     ('/. (('P a) b) body)
     (let* ((blam (process-/. `(/. ,b ,(preprocess body)) failure))
            (alam (process-/. `(/. ,a ,blam) failure)))
       `(/. ,v (((if (pair? ,v)) ((,alam (car ,v)) (cdr ,v))) (,failure ,v))))

     ('/. x body)
     (cond
      ((symbol? x) `(/. ,x ,(preprocess body)))
      ((or (number? x)
           (string? x)
           (symbol? x))
       `(/. ,v (((if (== ,v x)) ,(preprocess body)) (/. x FAIL))))
      (#t (err lam))))))

(define (ski e)
  (mtch
   e

   ;('/. (a . d) body) `(U ,(ski `(/. ,a (/. ,d ,body))))

   ('/. x (/. y b)) (ski `(/. ,x ,(ski `(/. ,y ,b))))

   ('/. x (a b)) `((S ,(ski `(/. ,x ,a))) ,(ski `(/. ,x ,b)))

   ('/. x y) (if (eq? x y) `I `(K ,y))

   (a b) (list (ski a) (ski b))

   ;(a . b) `((P ,(ski a)) ,(ski b))

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

   ('FAIL x) (err 'evl-step-FAIL e)

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

   ('pair? p) (let ((p (evl-fully p))) (mtch p (('P a) b) 'True x 'False))
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

(define (evl-fully e)
  (let ((e (evl e)))
    (mtch e
          (('P a) b) `((P ,(evl-fully a)) ,(evl-fully b))
          x x)))

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

;(tracefun evl evl-step evl-fully)
;(tracefun ski)
;(tracefun preprocess process-/./. process-/.)
