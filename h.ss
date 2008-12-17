(load "lib.ss")

(define use-ski #f)
(define show-tsgs #f)
(define show-bindings #f)

(define (forms->defs-n-tlfs tops)
  (mtch (group-by-preds (list (for def? fun?) (fnot (for def? fun?))) tops)
        (defs-n-funs tlfs)
        (let ((defs (map fun->def defs-n-funs)))
          (list defs tlfs))))

(define (run-src forms)
  (mtch (forms->defs-n-tlfs forms)
        (defs tlfs)
        (begin (map define-def (map preprocess defs))
               ;(shew global-env)
               (map evl-top tlfs (map preprocess tlfs)))))

(define (run-file filename)
  (run-src (append
            (read-objects "overture.ss")
            (read-objects filename))))

(define (cmpl-top e)
  (++ (render `(topevl ,(sdisplay e) ,(cmpl (preprocess e)))) ";\n"))
(define (crun-src tlfs)
  (cmd "rm -f obj.i vor")
  (write-string-to-file "obj.i" (apply ++ (map cmpl-top tlfs)))
  (cmd "make -s vor")
  (if (not (file-exists? "vor"))
      (err "No exe.")
      (cmd "./vor")))

(define (crun-file filename)
  (crun-src (append
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

(define (preprocess e)
  (mtch e
   ('def name e) `(def ,name ,(preprocess e))
   e (simplify (blunk (quote-ctors (doobie e))))))

(define global-env '())
(define (define-def e)
  (mtch e
        ('def name e)
        (set! global-env
              (cons (cons name (if use-ski
                                   (sski e)
                                   (vote-step e '()))) global-env))))
(define (dump-globals)
  (shew global-env))

(define (ski e)
  (mtch
   e

   ('/. x (/. y b)) (ski `(/. ,x ,(sski `(/. ,y ,b))))

   ('/. x (a b)) (simplify-ski `((S ,(ski `(/. ,x ,a))) ,(ski `(/. ,x ,b))))

   ('/. x y) (if (eq? x y) `I `(K ,y))

   (a b) (simplify-ski (list (ski a) (ski b)))

   ('P a b) `(P ,(ski a) ,(ski b))

   x (if (or (symbol? x) (number? x) (string? x)) x (err 'ski e))

   ))

(define sg (symbol-generator-generator))
(define tsg
  (let ((sg (tagged-symbol-generator-generator)))
    (lambda (tag . stuff)
      (let ((v (sg tag)))
        (if show-tsgs (shew `(,v ,tag . ,stuff)) '())
        v))))

(define (->/. e)
  (let ((v0 (sg))
        (v1 (sg))
        (v2 (sg)))
    (mtch
     e

     'S `(/. ,v0 (/. ,v1 (/. ,v2 ((,v0 ,v2) (,v1 ,v2)))))
     'K `(/. ,v0 (/. ,v1 ,v0))
     'I `(/. ,v0 ,v0)

     ;;; These shouldn't be necessary.
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
  (or (number? e) (quoted-symbol? e)))

(define (prim== a b)
  (mtch (list a b)
        (('quote a) ('quote b)) (eq? a b)
        x (smart== a b)))

(define (evl-step e)
  (mtch
   e

   ('FAIL x) (err 'evl-step-FAIL e)

   (('P a) b) (err 'fasdfasdf)
   ('P a b) e
   ('quote s) e

   ((('S f) g) x)
   `((,f ,x) (,g ,x))

   (('K x) y)
   x

   ('I x)
   x

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

   ('pair? p) (let ((p (evl-fully p))) (mtch p ('P a b) 'True x 'False))
   'pair? e

   (('cons a) b) `(P ,a ,b)

   ('car ('P a b)) a
   ('car x) `(car ,(evl x))

   ('cdr ('P a b)) b
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
          ('P a b) `(P ,(evl-fully a) ,(evl-fully b))
          x x)))

(define (evl-top src e)
  (display "+ ") (lshew src) (display "\n")
  (if use-ski
      (let ((ce (ski e)))
        (display "- ") (lshew ce) (display "\n")
        (let ((ee (evl-fully ce)))
          (display "=> ") (lshew ee) (display "\n")
          ee))

      (let ((ee (vote e)))
        (display "=> ") (lshew ee) (display "\n")
        (let ((se (simplify ee)))
                                        ;(display "\n")
          ee))))

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

(define (vote-step e env)
  (mtch
   e

   'FAIL (err e env)

   ('@ e env)
   (vote-step e env)

   ('FAIL x) (err 'evl-step-FAIL e)

   ('P a b) `(P ,(freeze a env) ,(freeze b env))

   ('/. a body)
   `($ ,e ,env)

   (('$ ('/. a body) env) v)
   (begin
     (if show-bindings (shew 'BIND a (terzie v)) '())
     (vote-step body (cons (cons a v) env)))

   'pair? e
   ('pair? e) (mtch (vote-fully e env) ('P a b) 'True x 'False)

   'True e
   'False e
   'Nil e

   ('quote s) s

   'car e
   ('car p) (mtch (vote-fully p env) ('P a b) a x (err 'not-pair e))

   'cdr e
   ('cdr p) (mtch (vote-fully p env) ('P a b) b x (err 'not-pair e))

   'if e
   ('if b) e
   (('if b) t) e
   ((('if b) th) el) (mtch (vote-fully b env) 'True (freeze th env) 'False (freeze el env))

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
   (('cons a) b) `(P (@ ,a ,env) ,(freeze b env))

   (a b) `(,(vote-completely a env) ,(freeze b env))

   x
   (cond
    ((symbol? x)
     (cond
      ((lookup-exists? x env) (lookup x env))
      ((lookup-exists? x global-env) (lookup x global-env))
      (#t (err 'unknown-variable x))))
    ((or (number? x) (string? x)) x)
    (#t (err 'vote-step e)))))

(define (vote-fully e env)
  (let ((ee (vote-step e env)))
    (cond
     ((or (data? ee) (equal? e ee)) ee)
     (#t (vote-fully ee env)))))

(define (vote-completely e env)
  (let ((e (vote-fully e env)))
    (mtch
     e

     ('P a b) `(P ,(vote-completely a env) ,(vote-completely b env))

     x x)))

(define (vote e)
  (vote-completely e '()))

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
   (('cons a) b) `((cons ,(doobie a)) ,(doobie b))
   ('car a) `(car ,(doobie a))
   ('cdr a) `(cdr ,(doobie a))

   (('+ a) b) `((+ ,(doobie a)) ,(doobie b))
   (('- a) b) `((- ,(doobie a)) ,(doobie b))
   (('* a) b) `((* ,(doobie a)) ,(doobie b))
   ((('if a) b) c) `(((if ,(doobie a)) ,(doobie b)) ,(doobie c))
   (('cons a) b) `((cons ,(doobie a)) ,(doobie b))

   ('/. args body) `(/. ,(doobie-arglist args) ,(doobie body))

   ('/./. . lams) `(/./. . ,(map doobie lams))

   (f . args) `(,(doobie f) ,(doobie-arglist (map-improper doobie args)))
   () 'Nil

   x (if (or (symbol? x) (number? x) (string? x)) x (err 'ski e))))

(define (build-receiver pat body)
  (mtch
   pat

   ('P a b) (build-receiver a (build-receiver b body))

   x (cond ((or (number? pat) (quoted-symbol? pat)) body)
           ((symbol? pat) `(/. ,x ,body))
           (#t (err 'build-receiver pat body)))))

(define (fuck pat failure)
  (let ((rv (tsg 'rec 'receiver pat))
        (v (tsg 'd 'traverser pat))
        (k (tsg 'k 'continuation pat)))
    (mtch
     pat

     ('P a b)
     (let ((lefter (fuck a failure))
           (righter (fuck b failure)))
       `(/. ,k (/. ,v (/. ,rv (((if (pair? ,v)) (((,lefter ((,righter ,k) (cdr ,v))) (car ,v)) ,rv)) ,failure)))))
     x (cond ((or (number? pat) (quoted-symbol? pat))
              `(/. ,k (/. ,v (/. ,rv (((if ((== ,v) ,pat)) (,k ,rv)) ,failure)))))
             ((symbol? pat)
              `(/. ,k (/. ,v (/. ,rv (,k (,rv ,v))))))
             (#t (err 'fucky pat failure))))))

(define (blunk-/. e failure)
  (let ((v (tsg 'b 'new-/. e)))
    (mtch e
          ('/. pat body) `(/. ,v (((,(fuck pat failure) (/. x x)) ,v) ,(build-receiver pat (blunk body)))))))

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

;; Environment inlining: not very tested.
;; (define (vank1 e env)
;;   (mtch env
;;         () e
;;         ((x . v) . rest) (vank1 (subst x v e) rest)))

;; (define (vank e)
;;   (mtch
;;    e
;;    ('@ e env) (vank1 e env)
;;    x x))

;; (define (gint1 e env)
;;   (mtch env
;;         () e
;;         ((x . v) . rest) `((/. ,x ,(gint1 e rest)) ,v)))

;; (define (gint e)
;;   (mtch
;;    e
;;    ('@ e env) (gint (gint1 e env))
;;    ('$ lam env) (gint (gint1 lam env))
;;    ('/. a b) `(/. ,a ,(gint b))
;;    (a b) `(,(gint a) ,(gint b))
;;    x x))

(define (cmpl e)
  (mtch
   e

   ('quote x) `(csymbol ,x)

   ('P a b) `(pair ,(cmpl a) ,(cmpl b))

   ('/. arg body) `(lambda ,(cmpl arg) ,(cmpl body))

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

(define (quote-ctors-def e)
  (mtch e
        ('def name e) `(def name ,(quote-ctors e))))

;(crun-file "src.ss")

;(tracefun evl evl-step evl-fully)
;(tracefun ski)
;(tracefun preprocess process-/./. process-/.)
;(tracefun build-receiver fuck)
;(tracefun vote vote-step)
;(tracefun vote-fully vote-completely)
;(tracefun doobie doobie doobie-arglist);(tracefun simplify simplify-env simplify-trivial-app)
;(tracefun blunk blunk-/./. blunk-/.)
;(tracefun simplify simplify-env simplify-trivial-app)
;(tracefun ->/.)
