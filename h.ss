(load "lib.ss")
(load "util.ss")

(define hen-version "* hen v. 0.01")

(define show-bindings #f)

(define (reset-everything)
  (clear-global-env))

(define (fun-name fun)
  (mtch fun
        ('fun (name . args) body) name))

(define (fun->/. fun)
  (mtch fun
        ('fun (name . args) body)
        `(/. ,args ,body)))

(define (funs->defs funs)
  (let ((blap (group-by fun-name funs)))
    (map (lambda (boo)
           (mtch boo
                 (name . funs)
                 `(def ,name (/./. . ,(map fun->/. funs)))))
         blap)))

(define (forms->defs-n-tlfs tops)
  (mtch (group-by-preds (list def? fun? (fnot (for def? fun?))) tops)
        (defs funs tlfs)
        (list (append defs (funs->defs funs)) tlfs)))

(define (import? form) (mtch form ('import . stuff) #t x #f))

(define (preprocess-program forms)
  (mtch (forms->defs-n-tlfs forms)
        (defs src-tlfs)
        (begin (map define-def (map preprocess defs))
               ;(shew< global-env)
               (list src-tlfs (map preprocess src-tlfs)))))

(define (run-src forms)
  (reset-everything)
  (count-reductions-start)
  (mtch (preprocess-program forms)
        (src-tlfs tlfs)
        (map evl-top src-tlfs tlfs))
  (count-reductions-end)
  (flush-output))

(define (run-file filename)
  (run-src (read-src filename)))

(define (read-src filename)
  (grep (fnot import?)
        (append
         (read-objects "overture.ss")
         (read-objects filename))))

(define (fun? e)
  (and (pair? e) (eq? (car e) 'fun)))

(define (def? e)
  (and (pair? e) (eq? (car e) 'def)))

(define (preprocess e)
  (mtch e
   ('def name e) `(def ,name ,(preprocess e))
   e (simplify (pattern-compile (doobie (syntax-desugar e))))))

(define global-env '())
(define (clear-global-env) (set! global-env '()))
(define (define-def e)
  (mtch e
        ('def name e)
        (set! global-env
              (cons (cons name (evl-step e '())) global-env))))

(define (evl-driver e)
;  (let ((ee (evl e)))
  (let ((ee e))
    (mtch ee
          ('P 'CommandSeq ('P command ('P kcommand 'Nil)))
          (let* ((r (evl-driver command))
                 (next-command (evl (list kcommand `(P ,r 'Nil)))))
            (evl-driver next-command))

          ('P 'Command ('P name ('P args 'Nil)))
          (execute-command name args)

          ('P 'Return ('P val 'Nil))
          val

;;           ('P 'CommandSeq ('P ('P 'Command ('P name ('P args 'Nil))) ('P k 'Nil)))
;;           (begin ;(shew 'yeah name args)
;;                  (let ((output (execute-command name args)))
;;                    ;(shew 'command-output output)
;;                    (evl-driver (list k `(P ,output 'Nil)))))

          x x)))

(define (evl-top src e)
  (display "+ ") (lshew src) (display "\n")
  (let ((ee (evl-driver (evl e))))
    (display "=> ") (plshew ee) (display "\n")
    ee))

(define (data? e)
  (mtch e
        ('Q q) #t
        ('P a b) #t
        ('$ lam env) #t
        x (or (number? e) (string? e) (symbol? e))))

(define (freeze e env)
  (mtch e
        ('@ e2 env2) e
        x `(@ ,e ,env)))

(define (terzie e)
  (mtch e
        ('@ e env) `(@ ,(terzie e))
        (a . b) (map terzie e)
        x x))

(define (evl-step e env)
  (set! n-reductions (+ n-reductions 1))
  (mtch
   e

   ; Maybe dont' need these
   ;'FAIL (err e env)
   ;('FAIL x) (err 'evl-step-FAIL e)

   ('@ e env) (evl-step e env)

   ('Q q) e

   ('P a b) `(P ,(freeze a env) ,(freeze b env))

   ('/. a body)
   `($ ,e ,env)

   (('$ ('/. a body) env) v)
   (begin
     (if show-bindings (shew 'BIND a (terzie v)) '())
     (evl-step body (cons (cons a v) env)))

   ('PAIR? e) (mtch (evl-fully e env) ('P a b) 'True x 'False)

   ('CAR p) (mtch (evl-fully p env) ('P a b) a x (err 'not-pair e))

   ('CDR p) (mtch (evl-fully p env) ('P a b) b x (err 'not-pair e))

   ((('if b) th) el) (mtch (evl-fully b env) 'True (freeze th env) 'False (freeze el env))

   (('== a) b) (mtch (smart== (evl-fully a env) (evl-fully b env)) #t 'True #f 'False)

   (a b) `(,(evl-completely a env) ,(freeze b env))

   x
   (cond
    ((ctor? x) x)
    ((symbol? x)
     (cond
      ((lookup-exists? x env) (lookup x env))
      ((lookup-exists? x global-env) (lookup x global-env))
      (#t (err 'unknown-variable x))))
    ((or (number? x) (string? x)) x)
    (#t (err 'evl-step e)))))

(define (evl-fully e env)
  (let ((ee (evl-step e env)))
    (cond
     ((or (data? ee) (equal? e ee)) ee)
     (#t (evl-fully ee env)))))

(define (evl-completely e env)
  (let ((e (evl-fully e env)))
    (mtch
     e

     ('P a b) `(P ,(evl-completely a env) ,(evl-completely b env))

     x x)))

(define (evl e)
  (evl-completely e '()))

(define (doobie-arglist args)
  (mtch
   args
   ;(a . d) `((P ,(doobie-arglist (car args))) ,(doobie-arglist (cdr args)))
   (a . d) `(P ,(car args) ,(doobie-arglist (cdr args)))
   () 'Nil
   x x))

(define (doobie e)
  (mtch
   e

   ('def name val) `(def ,name ,(doobie val))
   ('P a b) `(P ,(doobie a) ,(doobie b))
   ('CAR a) `(CAR ,(doobie a))
   ('CDR a) `(CDR ,(doobie a))

   ((('if a) b) c) `(((if ,(doobie a)) ,(doobie b)) ,(doobie c))

   ('/. args body) `(/. ,(doobie-arglist args) ,(doobie body))

   ('/./. . lams) `(/./. . ,(map doobie lams))

   (f . args) `(,(doobie f) ,(doobie-arglist (map-improper doobie args)))
   () 'Nil

   x (if (or (symbol? x) (number? x) (string? x)) x (err 'doobie e))))

(define (build-receiver pat body)
  (mtch
   pat

   ('P a b) (build-receiver a (build-receiver b body))

   x (cond ((or (number? pat) (quoted-symbol? pat)) body)
           ((symbol? pat) `(/. ,x ,body))
           (#t (err 'build-receiver pat body)))))

(define (build-traverser pat failure)
  (let ((rv (tsg 'rec 'receiver pat))
        (v (tsg 'd 'traverser pat))
        (k (tsg 'k 'continuation pat)))
    (mtch
     pat

     ('P a b)
     (let ((lefter (build-traverser a failure))
           (righter (build-traverser b failure)))
       `(/. ,k (/. ,v (/. ,rv (((if (PAIR? ,v)) (((,lefter ((,righter ,k) (CDR ,v))) (CAR ,v)) ,rv)) ,failure)))))
     x (cond ((or (number? pat) (quoted-symbol? pat))
              `(/. ,k (/. ,v (/. ,rv (((if ((== ,v) ,pat)) (,k ,rv)) ,failure)))))
             ((symbol? pat)
              `(/. ,k (/. ,v (/. ,rv (,k (,rv ,v))))))
             (#t (err 'build-traverser pat failure))))))

(define (pattern-compile-/. e failure)
  (let ((v (tsg 'b 'new-/. e)))
    (mtch e
          ('/. pat body) `(/. ,v (((,(build-traverser pat failure) (/. x x)) ,v) ,(build-receiver pat (pattern-compile body)))))))

(define (pattern-compile-/./. lams)
  (let ((v (tsg 'bb '/./. lams))
        (failure-v (tsg 'f '/./.-failure)))
    (if (null? lams)
        `(/. ,v (FAIL 1))
        `(/. ,v
             ((/. ,failure-v (,(pattern-compile-/. (car lams) failure-v) ,v))
              (,(pattern-compile-/./. (cdr lams)) ,v))))))

(define (pattern-compile e)
  (mtch e
        ('/. pat body) (pattern-compile-/. e 'TOPFAIL)
        ('/./. . lams) (pattern-compile-/./. lams)
        ('P a b) `(P ,(pattern-compile a) ,(pattern-compile b))

        ('== a) `(== ,(pattern-compile a))
        (('== a) b) `((== ,(pattern-compile a)) ,(pattern-compile b))

        ((('if b) t) e) `(((if ,(pattern-compile b)) ,(pattern-compile t)) ,(pattern-compile e))

        'True e
        'False e
        'Nil e

        (a b) `(,(pattern-compile a) ,(pattern-compile b))
        x (cond
           ((symbol? e) e)
           ((number? e) e)
           ((quoted-symbol? e) e)
           ((string? e) e)
           (#t (err 'pattern-compile e)))))

(define (subst x v e)
  (mtch
   e

   ('/. xx body)
   (if (eq? xx x)
       e
       `(/. ,xx ,(subst x v body)))

   ('P a b) `(P ,(subst x v a) ,(subst x v b))

   (a b) `(,(subst x v a) ,(subst x v b))

   xx (cond
       ((eq? x xx) v)
       ((or (symbol? e) (number? e) (quoted-symbol? e)) e)
       (#t (err 'subst x v e)))))

(define (occurs x body)
  (mtch
   body

   (/. xx body)
   (if (eq? xx x)
       0
       (occurs x body))

   (a b)
   (+ (occurs x a) (occurs x b))

   xx (if (eq? x xx) 1 0)))

(define (simplify-trivial-app e)
  (mtch
   e

   (('/. x body) v)
   (if (symbol? x)
       (if (or (symbol? v) (= (occurs x body) 1))
           (subst x v body)
           e)
       e)
   x x))

(define (simplify-env e)
  (if (null? e)
      '()
      (cons (cons (caar e) (simplify (cdar e))) (simplify-env (cdr e)))))

(define (simplify e)
  (mtch
   e

   ('$ lam env) `($ ,(simplify lam) ,(simplify-env env))

   (('/. pat body) v) (mtch (simplify-trivial-app e)
                            (('/. a b) c)
                            (list (simplify `(/. ,a ,b)) (simplify c))
                            x
                            (simplify x))

   ('/. pat body) `(/. ,pat ,(simplify body))
   (a b) (let ((ee `(,(simplify a) ,(simplify b))))
           (if (equal? e ee)
               ee
               (simplify ee)))
   ;(simplify `(,(simplify a) ,(simplify b)))

   x x))

; P isn't really sugar, since you can't use P directly, but whatever.
(define (syntax-sugar e) (un-cons-ify (un-p-ify e)))
(define (syntax-desugar e) (p-ify-ctons (cons-ify e)))

(define (hen args)
  (map run-file args))
