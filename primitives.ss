(define primitive-env '())

(define (define-primitive name f)
  (set! primitive-env
        (cons (cons name f) primitive-env))
  (global-env-define name `(@ ,name)))

(define (blimpp f args)
  (let ((prim-f (assoc f primitive-env)))
    (if (eq? prim-f #f)
        (err 'blimpp f)
        (apply (cdr prim-f) args))))

(define-primitive '+
  (lambda (a b)
    (+ a b)))
(define-primitive '-
  (lambda (a b)
    (- a b)))
(define-primitive '*
  (lambda (a b)
    (* a b)))
(define-primitive '/
  (lambda (a b)
    (/ a b)))

(define (tfy x)
  (cond
   ((eq? #f x) 'False)
   ((eq? #t x) 'True)
   (#t (err 'tfy x))))

(define-primitive 'untfy
  (lambda (x)
    (cond
     ((eq? x 'True) #t)
     ((eq? x 'False) #f)
     (#t (err 'untfy x)))))

(define-primitive 'tfall?
  (lambda (lyst)
    (all? (map untfy lyst))))

(define-primitive '==
  (lambda (a b)
    (let ((ta (typeof a))
          (tb (typeof b)))
      (tfy (and (eq? ta tb)
                (cond
                 ((eq? 'cton ta)
                  (if (= (length a) (length b))
                      (tfall? (map == a b))
                      #f))
                 ((eq? 'string ta) (string= a b))
                 ((eq? 'number ta) (= a b))
                 ((eq? 'ctor ta) (eq? a b))
                 ((eq? 'symbol ta) (eq? a b))
                 (#t (err '== a b))))))))
;(tracefun == all?)

(define-primitive '>
  (lambda (a b)
    (let ((a a)
          (b b))
      (tfy (and (number? a) (number? b) (> a b))))))

(define-primitive 'read-file
  (lambda (filename)
    (sb-consyize (sb-read-file filename))))

(define-primitive '__prim-shew
  (lambda (e)
    (shew e)
    'Mu))

(define-primitive 'list
  (lambda args args))
