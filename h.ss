(load "lib.ss")

(define eval-steps 0)

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
  ;(set! eval-steps 0)
  (let* ((forms (map fun->def forms))
         (defs (map preprocess-def (grep def? forms)))
         ;(defs (map doobie defs))
         (tlfs (map preprocess (grep (fnot def?) forms))))
    (map define-def defs)
    ;(dump-globals)
    (map evl-top tlfs)
    ;(shew (list 'steps eval-steps))
    ))

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
;              (cons (cons name (sski e)) global-env))))
              (cons (cons name e) global-env))))
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
  (let ((v (sg)))
    (if (null? lams)
        `(/. x FAIL)
        `(/. ,v (,(process-/. (car lams)
                              `(,(process-/./. (cdr lams)) ,v))
                 ,v)))))

(define (process-/. lam failure)
  (let ((v (sg)))
    (mtch
     lam

     ('/. (('P a) b) body)
     (let* ((blam (process-/. `(/. ,b ,(preprocess body)) failure))
            (alam (process-/. `(/. ,a ,blam) failure)))
       `(/. ,v (((if (pair? ,v)) ((,alam (car ,v)) (cdr ,v))) ,failure)))

     ('/. x body)
     (cond
      ((symbol? x) `(/. ,x ,(preprocess body)))
      ((or (number? x)
           (string? x))
       `(/. ,v (((if ((== ,v) ,x)) ,(preprocess body)) ,failure)))
      ((quoted-symbol? x)
       `(/. ,v (((if ((== ,v) ,x)) ,(preprocess body)) ,failure)))
      (#t (err lam))))))

(define (ski e)
  (mtch
   e

   ;('/. (a . d) body) `(U ,(ski `(/. ,a (/. ,d ,body))))

   ('/. x (/. y b)) (ski `(/. ,x ,(sski `(/. ,y ,b))))

   ('/. x (a b)) (simplify-ski `((S ,(ski `(/. ,x ,a))) ,(ski `(/. ,x ,b))))

   ('/. x y) (if (eq? x y) `I `(K ,y))

   (a b) (simplify-ski (list (ski a) (ski b)))

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

(define (prim== a b)
  (mtch (list a b)
        (('quote a) ('quote b)) (eq? a b)
        x (smart== a b)))

(define (evl-step e)
  (set! eval-steps (1+ eval-steps))
  (mtch
   e

   ('FAIL x) (err 'evl-step-FAIL e)

   (('P a) b) e
   ('quote s) e

   ;(('U f) (('P x) y))
   ;`((,f ,x) ,y)

   ((('S f) g) x)
   `((,f ,x) (,g ,x))

   (('K x) y)
   x

   ('I x)
   x

   ('P a) e
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

   ('dbg v)
   (begin
     (shew (list 'DBG v))
     v)

   'dbg e

   (('+ a) b) (+ (evl a) (evl b))
   (('- a) b) (- (evl a) (evl b))
   (('* a) b) (* (evl a) (evl b))
   (('== a) b) (if (prim== (evl a) (evl b)) 'True 'False)
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
  (display "+ ") (lshew e) (display "\n")
;  (let ((ce (sski e)))
;    (display "- ") (lshew ce) (display "\n")
    (let ((ee (vote e)))
      (display "=> ") (lshew ee) (display "\n\n")
      ee)
;)
)

(define (simplify-ski-step e)
  (mtch
   e

   (('S ('K 'I)) joe) joe
   (('S ('K a)) ('K b)) `(K (,a ,b))
   (('S ('S 'K)) blah) blah

;;    ;(('S 'K) a) 'I
;;    ;('I 'I) 'I
;;    ;(S (S S) (K K))) 'K

;;    ;(((a b) c) d) `((,(simplify-ski `(,a ,b)) ,c)  ,d)

    (a b) (list (simplify-ski a) (simplify-ski b))

   'S e
   'K e
   'I e

   x x
))

(define (simplify-ski e)
  (let ((ee (simplify-ski-step e)))
    (if (equal? e ee)
        ee
        (simplify-ski ee))))

(define (sski e) (simplify-ski (ski e)))

;; (define (doobie def)
;;   (mtch
;;    def
;;    ('def name val) `(def ,name ,(doobie-exp val))))

;; (define (doobie-arglist args)
;;   (if (pair? args)
;;       `((P ,(car args)) ,(doobie-arglist (cdr args)))
;;       args))

;; (define (doobie-exp e)
;;   (mtch
;;    e

;;    ('/. args body) `(/. ,(doobie-arglist args) ,(doobie-exp body))

;;    ;(a b) (list (doobie-exp a) (doobie-exp b))
;;    (f . args) `(,(doobie-exp f) ,(doobie-arglist args))

;;    x (if (or (symbol? x) (number? x) (string? x)) x (err 'ski e))))

;; ;(shew (doobie-exp '(/. (x . joe) x)))
;; ;(shew (doobie-exp '(/. (x y z . q) x)))
;; ;(shew (doobie-exp '(f . 1)))
;; ;(shew (doobie-exp '(f 1 . 2)))
;; ;(shew (doobie-exp '(f 1 2 . 3)))

;; (shew (doobie-exp '(/. (a b . c) (+ a c))))
;; (shew (doobie-exp '(ulp 1 2 . 3)))
;; (shew (doobie-exp '((/. (a b . c) ((+ a) c)) 1 2 . 3)))

;(tracefun evl evl-step evl-fully)
;(tracefun ski)
;(tracefun preprocess process-/./. process-/.)

(define (data? e)
  (mtch e
        (('P a) b) #t
        ('$ lam env) #t
        x (or (number? e) (string? e))))
;(tracefun data?)

(define (vote-step e env)
  (mtch
   e

   'FAIL (err e env)

   ('@ e env)
   (vote-step e env)

   ('FAIL x) (err 'evl-step-FAIL e)

   (('P a) b) `((P (@ ,a ,env)) (@ ,b ,env))

   ('/. a body)
   `($ ,e ,env)

   (('$ ('/. a body) env) v)
   (vote-step body (cons (cons a v) env))

   'pair? e
   ('pair? e) (mtch (vote-fully e env) (('P a) b) 'True x 'False)

   'True e
   'False e
   'Nil e

   ('quote s) e

   'car e
   ('car p) (mtch (vote-fully p env) (('P a) b) a x (err 'not-pair e))

   'cdr e
   ('cdr p) (mtch (vote-fully p env) (('P a) b) b x (err 'not-pair e))

   'if e
   ('if b) e
   (('if b) t) e
   ((('if b) th) el) (mtch (vote-fully b env) 'True `(@ ,th ,env) 'False `(@ ,el ,env))

   '+ e
   ('+ a) e
   (('+ a) b) (+ (vote-fully a env) (vote-fully b env))

   '- e
   ('- a) e
   (('- a) b) (- (vote-fully a env) (vote-fully b env))

   '* e
   ('* a) e
   (('* a) b) (* (vote-fully a env) (vote-fully b env))

   '== e
   ('== a) e
   (('== a) b) (mtch (prim== (vote-fully a env) (vote-fully b env)) #t 'True #f 'False)

   'cons e
   ('cons a) e
   (('cons a) b) `((P (@ ,a ,env)) (@ ,b ,env))

   (a b) `(,(vote-completely a env) (@ ,b ,env))

   x
   (cond
    ((symbol? x)
     (cond
      ((lookup-exists? x env) (lookup x env))
      ((lookup-exists? x global-env) (lookup x global-env))
      (#t (err 'unknown-variable x))))
    ((or (number? x) (string? x)) x)
    (#t (err 'evl e)))))

(define (vote-fully e env)
  (let ((ee (vote-step e env)))
    (cond
     ((or (data? ee) (equal? e ee)) ee)
     (#t (vote-fully ee env)))))

(define (vote-completely e env)
  (let ((e (vote-fully e env)))
    (mtch
     e

     (('P a) b) `((P ,(vote-completely a env)) ,(vote-completely b env))

     x x)))
;;      (begin
;;        (assert (data? x))
;;        x))))

(define (vote e)
  (vote-completely e '()))

;(tracefun vote vote-step)
;(tracefun vote-fully vote-completely)
;; (shew (process-/. '(/. x x) 'FAIL))
;; (shew (process-/./. (list '(/. x x))))
;; (shew (process-/./. (list '(/. ((P a) b) a))))
