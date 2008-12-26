(load "lib.ss")

(define hen-version "* hen v. 0.01")

(define count-reductions #f)
(define show-tsgs #f)
(define show-bindings #f)
(define pretty-output #t)

(define n-reductions 0)
(define (count-reductions-start)
  (set! n-reductions 0))
(define (count-reductions-end)
  (if count-reductions
      (begin
        (display n-reductions)
        (display " reductions.\n"))
      '()))

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
  (count-reductions-start)
  (mtch (preprocess-program forms)
        (src-tlfs tlfs)
        (map evl-top src-tlfs tlfs))
  (count-reductions-end))

(define (run-file filename)
  (run-src (read-src filename)))

(define (read-src filename)
  (append
   (read-objects "overture.ss")
   (read-objects filename)))

(define (cmpl-def def)
  (mtch def
        (name . e)
        (++ (render `(store_global ,name ,(cmpl e))) ";\n")))

(define (cmpl-top src-e e)
  (++ (render `(evl_top ,(sdisplay src-e) ,(cmpl e))) ";\n"))


(define (csrc->obj forms)
  (mtch (preprocess-program forms)
        (src-tlfs tlfs)
        (++ "#include \"vor.h\"\n"
            "void hen_main() { "
            (apply ++ (append
                       (map cmpl-def global-env)
                       (map cmpl-top src-tlfs tlfs)))
            "}")))

(define stdobjs "spew.o mem.o")
(define moreobjs "fbo.o GLee.o")
(define libs "-framework GLUT -framework OpenGL -framework CoreFoundation")

(define (cbuild-exe stub objcfile objfile exefile)
  (cmd (++ "rm -f " objfile exefile))
  (cmd (++ "make -s vor.o " stdobjs " " moreobjs))
  (scmd (++ "gcc -std=c99 -g -o " exefile " vor.o " objcfile " " stdobjs " " moreobjs " " libs)))

(define (crun-file srcfile run-p delete-p)
  (shew 'run run-p)
  (shew 'delete-p delete-p)
  (let* ((objcfile (++ srcfile ".c"))
         (objfile (++ srcfile ".o"))
         (stub (remove-extension srcfile))
         (exefile stub))
    (cmd (++ "rm -f " objcfile " " exefile))
    (write-string-to-file objcfile (csrc->obj (read-src srcfile)))
    (cbuild-exe stub objcfile objfile exefile)
    (if (not (file-exists? exefile))
        (err "No exe.")
        (begin
          (if run-p
              (cmd (++ "./" exefile))
              '())
          (if delete-p
              (cmd (++ "rm " exefile))
              '())))))

(define (fun? e)
  (and (pair? e) (eq? (car e) 'fun)))

(define (def? e)
  (and (pair? e) (eq? (car e) 'def)))

(define (opaque v) `(Q ,v))
(define (opaque-val q)
  (assert (opaque? q))
  (cadr q))

(define (opaque? e)
  (mtch e
        ('Q q) #t
        x #f))

(define (preprocess e)
  (mtch e
   ('def name e) `(def ,name ,(preprocess e))
   e (simplify (pattern-compile (quote-ctors (doobie (syntax-desugar e)))))))

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

(define (hcar e) (mtch e ('P 'Cons ('P a ('P d 'Nil))) a))
(define (hcdr e) (mtch e ('P 'Cons ('P a ('P d 'Nil))) d))
(define (hcadr e) (hcar (hcdr e)))

(define (create-ref v) (opaque (box v)))
(define (read-ref r) (unbox (opaque-val r)))
(define (write-ref rv) (begin (set-box! (opaque-val (hcar rv)) (hcadr rv)) 'Nil))
(define (destroy-ref r) ''Nil)

(define (execute-command name arg)
  (display "Command: ")
  (display name)
  (display " ")
  (plshew arg)
  (display "\n")
  (mtch name
        'shew (begin (shew (list 'SHEW arg)) 'Nil)
        'create-ref (create-ref arg)
        'read-ref (read-ref arg)
        'write-ref (write-ref arg)
        'destroy-ref (destroy-ref arg)
        x (err "Unknown command" (list name arg))))

(define (evl-driver e)
  (let ((ee (evl e)))
    (mtch ee
          ('P 'X ('P name ('P arg ('P k 'Nil))))
          (let ((output (execute-command name arg)))
            (evl-driver (list k `(P ,output 'Nil))))
          
          x x)))

(define (evl-top src e)
  (display "+ ") (lshew src) (display "\n")
  (let ((ee (evl-driver e)))
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

   ('quote s) s

   ('CAR p) (mtch (evl-fully p env) ('P a b) a x (err 'not-pair e))

   ('CDR p) (mtch (evl-fully p env) ('P a b) b x (err 'not-pair e))

   ((('if b) th) el) (mtch (evl-fully b env) 'True (freeze th env) 'False (freeze el env))

   (('+ a) b) (+ (evl-fully a env) (evl-fully b env))

   (('- a) b) (- (evl-fully a env) (evl-fully b env))

   (('* a) b) (* (evl-fully a env) (evl-fully b env))

   (('== a) b) (mtch (prim== (evl-fully a env) (evl-fully b env)) #t 'True #f 'False)

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
   ('CAR a) `(CAR ,(doobie a))
   ('CDR a) `(CDR ,(doobie a))

   (('+ a) b) `((+ ,(doobie a)) ,(doobie b))
   (('- a) b) `((- ,(doobie a)) ,(doobie b))
   (('* a) b) `((* ,(doobie a)) ,(doobie b))
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

        ('+ a) `(+ ,(pattern-compile a))
        (('+ a) b) `((+ ,(pattern-compile a)) ,(pattern-compile b))

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

(define (p-ify-ctons e)
  (cond
   ((cton? e) (p-ify (map p-ify-ctons e)))
   ((pair? e) (map-improper p-ify-ctons e))
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
        (a . b) (map un-cons-ify e)
        x x))

(define (un-cons-ify-1 e)
  (mtch e
        ('Cons a b) (cons a (un-cons-ify-1 b))
        'Nil '()
        x x))

; P isn't really sugar, since you can't use P directly, but whatever.
(define (syntax-sugar e) (un-cons-ify (un-p-ify e)))
(define (syntax-desugar e) (p-ify-ctons (cons-ify e)))

(define (prettify-shewer shewer)
  (lambda args (apply shewer ((if pretty-output syntax-sugar id) args))))

(define pshew (prettify-shewer shew))
(define plshew (prettify-shewer lshew))

;(tracefun preprocess)
;(tracefun build-receiver build-traverser)
;(tracefun evl evl-step)
;(tracefun evl-fully evl-completely)
;(tracefun doobie doobie doobie-arglist)
;(tracefun pattern-compile pattern-compile-/./. pattern-compile-/.)
;(tracefun simplify simplify-env simplify-trivial-app)
;(tracefun render)
;(tracefun cmpl cmpl-def)
;(tracefun syntax-desugar syntax-sugar p-ify un-p-ify)
;(tracefun cons-ify un-cons-ify un-cons-ify-1)
;(tracefun evl-driver execute-command)

(define (usage)
  (display (++
            hen-version "\n"
            "Usage:\n"
            "  hen [file]           : compile and run (and delete exe)\n"
            "  hen interpret [file] : run interpreter on file\n"
            "  hen compile [file]   : compile but don't run file\n")))

;;             "Usage: hen options src [src...]\n"
;;             "  -n: use interpreter\n"
;;             "  -c: just generate exe (otherwise run and delete)\n")))

(define (hen args)
  (mtch args
        () (usage)
        ("interpret" filename) (run-file filename)
        ("compile" filename) (crun-file filename #f #f)
        (filename) (crun-file filename #t #t)))
