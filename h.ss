(load "lib.ss")

(define show-tsgs #f)
(define show-bindings #f)
(define pretty-output #t)

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

(define (preprocess-program forms)
  (mtch (forms->defs-n-tlfs forms)
        (defs src-tlfs)
        (begin (map define-def (map preprocess defs))
               ;(shew global-env)
               (list src-tlfs (map preprocess src-tlfs)))))

(define (run-src forms)
  (mtch (preprocess-program forms)
        (src-tlfs tlfs)
        (map evl-top src-tlfs tlfs)))

(define (run-file filename)
  (run-src (read-src filename)))

(define (read-src filename)
;  (pretty-ugly
(append
                (read-objects "overture.ss")
                (read-objects filename)))
;)

(define (cmpl-def def)
  (mtch def
        (name . e)
        (++ (render `(store_global ,name ,(cmpl e))) ";\n")))

(define (cmpl-top src-e e)
  (++ (render `(topevl ,(sdisplay src-e) ,(cmpl e))) ";\n"))

(define (crun-src forms)
  (mtch (preprocess-program forms)
        (src-tlfs tlfs)
        (crun-obj (apply ++ (append
                             (map cmpl-def global-env)
                             (map cmpl-top src-tlfs tlfs))))))

(define (crun-obj tlfs)
  (flush-output)
  (cmd "rm -f obj.i vor")
  (write-string-to-file "obj.i" tlfs)
  (cmd "make vor")
  (if (not (file-exists? "vor"))
      (err "No exe.")
      (cmd "./vor")))

(define (crun-file filename)
  (crun-src (read-src filename)))

(define (fun? e)
  (and (pair? e) (eq? (car e) 'fun)))

(define (def? e)
  (and (pair? e) (eq? (car e) 'def)))

(define (preprocess e)
  (mtch e
   ('def name e) `(def ,name ,(preprocess e))
   e (simplify (blunk (quote-ctors (doobie (preprocess-ctons (cons-ify e))))))))

(define global-env '())
(define (define-def e)
  (mtch e
        ('def name e)
        (set! global-env
              (cons (cons name (evl-step e '())) global-env))))

(define sg (symbol-generator-generator))
(define tsg
  (let ((sg (tagged-symbol-generator-generator)))
    (lambda (tag . stuff)
      (let ((v (sg tag)))
        (if show-tsgs (shew `(,v ,tag . ,stuff)) '())
        v))))

(define (prim== a b)
  (mtch (list a b)
        (('quote a) ('quote b)) (eq? a b)
        x (smart== a b)))

(define (evl-top src e)
  (display "+ ") (lshew src) (display "\n")
  (let ((ee (evl e)))
    (display "=> ") (plshew ee) (display "\n")
    ee))

(define (data? e)
  (mtch e
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
  (mtch
   e

   'FAIL (err e env)

   ('@ e env)
   (evl-step e env)

   ('FAIL x) (err 'evl-step-FAIL e)

   ('P a b) `(P ,(freeze a env) ,(freeze b env))

   ('/. a body)
   `($ ,e ,env)

   (('$ ('/. a body) env) v)
   (begin
     (if show-bindings (shew 'BIND a (terzie v)) '())
     (evl-step body (cons (cons a v) env)))

   'PAIR? e
   ('PAIR? e) (mtch (evl-fully e env) ('P a b) 'True x 'False)

   'True e
   'False e
   'Nil e

   ('quote s) s

  'CAR e
  ('CAR p) (mtch (evl-fully p env) ('P a b) a x (err 'not-pair e))

  'CDR e
  ('CDR p) (mtch (evl-fully p env) ('P a b) b x (err 'not-pair e))

   'if e
   ('if b) e
   (('if b) t) e
   ((('if b) th) el) (mtch (evl-fully b env) 'True (freeze th env) 'False (freeze el env))

   '+ e
   ('+ a) e
   (('+ a) b) (+ (evl-fully a env) (evl-fully b env))

   '- e
   ('- a) e
   (('- a) b) (- (evl-fully a env) (evl-fully b env))

   '* e
   ('* a) e
   (('* a) b) (* (evl-fully a env) (evl-fully b env))

   '== e
   ('== a) e
   (('== a) b) (mtch (prim== (evl-fully a env) (evl-fully b env)) #t 'True #f 'False)

;   'cons e
;   ('cons a) e
;   (('cons a) b) `(P (@ ,a ,env) ,(freeze b env))

   (a b) `(,(evl-completely a env) ,(freeze b env))

   x
   (cond
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
   ('quote x) e
   ('P a b) `(P ,(doobie a) ,(doobie b))
   (('+ a) b)  `((+ ,(doobie a)) ,(doobie b))
;   (('cons a) b) `((cons ,(doobie a)) ,(doobie b))
  ('CAR a) `(CAR ,(doobie a))
  ('CDR a) `(CDR ,(doobie a))

   (('+ a) b) `((+ ,(doobie a)) ,(doobie b))
   (('- a) b) `((- ,(doobie a)) ,(doobie b))
   (('* a) b) `((* ,(doobie a)) ,(doobie b))
   ((('if a) b) c) `(((if ,(doobie a)) ,(doobie b)) ,(doobie c))
;   (('cons a) b) `((cons ,(doobie a)) ,(doobie b))

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

(define (blunk-/. e failure)
  (let ((v (tsg 'b 'new-/. e)))
    (mtch e
          ('/. pat body) `(/. ,v (((,(build-traverser pat failure) (/. x x)) ,v) ,(build-receiver pat (blunk body)))))))

(define (blunk-/./. lams)
  (let ((v (tsg 'bb '/./. lams))
        (failure-v (tsg 'f '/./.-failure)))
    (if (null? lams)
        `(/. ,v (FAIL 1))
        `(/. ,v
             ((/. ,failure-v (,(blunk-/. (car lams) failure-v) ,v))
              (,(blunk-/./. (cdr lams)) ,v))))))

(define (blunk e)
  (mtch e
        ('/. pat body) (blunk-/. e 'TOPFAIL)
        ('/./. . lams) (blunk-/./. lams)
        ('P a b) `(P ,(blunk a) ,(blunk b))

        ('+ a) `(+ ,(blunk a))
        (('+ a) b) `((+ ,(blunk a)) ,(blunk b))

        ('== a) `(== ,(blunk a))
        (('== a) b) `((== ,(blunk a)) ,(blunk b))

        ((('if b) t) e) `(((if ,(blunk b)) ,(blunk t)) ,(blunk e))

        'True e
        'False e
        'Nil e

        (a b) `(,(blunk a) ,(blunk b))
        x (cond
           ((symbol? e) e)
           ((number? e) e)
           ((quoted-symbol? e) e)
           (#t (err 'blunk e)))))

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
       ((symbol? e) e)
       ((or (number? e) (quoted-symbol? e)) e)
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

(define (cmpl e)
  (mtch
   e

   () '(nil)

   ('quote x) `(csymbol ,x)

   ('P a b) `(pair ,(cmpl a) ,(cmpl b))

   ('/. arg body) `(lambda ,(cmpl arg) ,(cmpl body))

   ('$ lam env) `(closure ,(cmpl lam) ,(cmpl env))

   (a b) `(app ,(cmpl a) ,(cmpl b))

   x (cond
      ;((quoted-symbol? x) `(csymbol ,x))
      ((symbol? x) `(symbol ,x))
      ((integer? x) `(integer ,x))
      (#t (err 'cmpl x)))))

(define (render e)
  (cond
   ((pair? e) (++ (car e) "(" (join-things ", " (map render (cdr e))) ")"))
   ((symbol? e) (++ #\" e #\"))
   ((or (symbol? e) (number? e)) (->string e))
   ((string? e) (++ "\"" e "\""))
   (#t (err 'render e))))

(define (quote-ctors e)
  (atom-traverse (lambda (p) (if (and (ctor? p) (not (eq? p 'P))) `(quote ,p) p)) e))

(define (preprocess-ctons e)
  (cond
   ((cton? e) (syntax-desugar (map-improper preprocess-ctons e)))
   ((pair? e) (map-improper preprocess-ctons e))
   (#t e)))

(define (p-ify e)
  (mtch e
        '() 'Nil
        (a . b) `(P ,a ,(p-ify b))
        x x))

(define (un-p-ify e)
  (mtch e
        ('P a 'Nil) (list (un-p-ify a))
        ('P a b) (cons (un-p-ify a) (un-p-ify b))
        (a . b) (map un-p-ify e)
        x x))

(define (cons-ify e)
  (mtch e
        ('$) 'Nil
        ('$ a . d) `(Cons ,(cons-ify a) ,(cons-ify `($ . ,d)))
        (a . d) (map-improper cons-ify e)
        x x))

(define (un-cons-ify e)
  (mtch e
        ('Cons a b) `($ . ,(map-improper un-cons-ify (un-cons-ify-1 e)))
        (a . b) (map-improper un-cons-ify e)
        x x))

(define (un-cons-ify-1 e)
  (mtch e
        ('Cons a b) (cons a (un-cons-ify-1 b))
        'Nil '()
        x x))

; P isn't really sugar, since you can't use P directly, but whatever.
(define (syntax-sugar e) (un-cons-ify (un-p-ify e)))
(define (syntax-desugar e) (p-ify (cons-ify e)))

(define (prettify-shewer shewer)
  (lambda args (apply shewer ((if pretty-output syntax-sugar id) args))))

(define pshew (prettify-shewer shew))
(define plshew (prettify-shewer lshew))

;(tracefun preprocess)
;(tracefun build-receiver build-traverser)
;(tracefun evl evl-step)
;(tracefun evl-fully evl-completely)
;(tracefun doobie doobie doobie-arglist)
;(tracefun blunk blunk-/./. blunk-/.)
;(tracefun simplify simplify-env simplify-trivial-app)
;(tracefun render)
;(tracefun cmpl cmpl-def)
;(tracefun preprocess-ctons syntax-desugar syntax-sugar p-ify un-p-ify)
;(tracefun cons-ify un-cons-ify un-cons-ify-1)
