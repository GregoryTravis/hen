(load "lib.ss")

(define hen-version "* hen v. 0.01")

(define remove-temporaries #f)
(define count-reductions #f)
(define show-tsgs #f)
(define show-bindings #f)
(define pretty-output #t)
(define show-commands #f)

(define (rmtemps . files)
  (if (not (null? files))
      (if remove-temporaries
          (srcmd (join-things " " (cons 'rm files)))
          '())
      '()))
(define (rm-rtemps . files)
  (if remove-temporaries
      (srcmd (join-things " " (cons 'rm (cons '-r files))))
      '()))

(define (reset-everything)
  (clear-global-env))

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
         (read-objects "shew.stub.ss")
         (read-objects filename))))

(define (cmpl-def def)
  (mtch def
        (name . e)
        (++ (render `(store_global ,name ,(cmpl e))) ";\n")))

(define (cmpl-top src-e e)
  (++ (render `(evl_top ,(sdisplay src-e) ,(cmpl e))) ";\n"))

(define (generate-registration-includes modules)
  (map ($ ++ "#include \"" _ ".impl.h\"\n") modules));(map c-identifier-safe modules)))

(define (generate-blott-decls modules)
  (map ($ ++ "extern void " _ "_blott();\n") (map c-identifier-safe modules)))

(define (generate-registration-calls modules)
  (map ($ ++ _ "_impl_register();\n") (map c-identifier-safe modules)))

(define (generate-blott-calls modules)
  (map ($ ++ _ "_blott();\n") (map c-identifier-safe modules)))

(define (csrc->obj modules forms stub)
  (mtch (preprocess-program forms)
        (src-tlfs tlfs)
        (++ "#include \"vor.h\"\n"
;            (apply ++ (generate-registration-includes modules))
            "void " (c-identifier-safe stub) "_blott() { "
;            (apply ++ (generate-registration-calls modules))
            (apply ++ (append
                       (map cmpl-def global-env)
                       (map cmpl-top src-tlfs tlfs)))
            "}")))

(define (cleanup-module-stuff)
  (map (lambda (module)
         (rmtemps (++ module ".impl.c") (++ module ".impl.h")))
       modules))

(define (compile-ss-to-c modules srcfile objcfile stub)
  (write-string-to-file objcfile (csrc->obj modules (read-src srcfile) stub)))

(define (get-imports-from-file src)
  (apply append (map cdr (grep import? (read-objects src)))))

(define (compile filename) (crun-file filename #f #f))
(define (crun filename) (crun-file filename #t #t))
(define (interpret filename) (run-file filename))
(define run run-file)

(define (c-identifier-safe s)
  (string-replace-char-pairs s '((#\- . #\_))))

(define (gen-main stub modules file)
  (write-string-to-file
   file
   (++ "#include \"vor.h\"\n"
       (apply ++ (generate-registration-includes modules))
       (apply ++ (generate-blott-decls (map c-identifier-safe (snoc modules stub))))
;       (apply ++ (generate-blott-decls (map c-identifier-safe modules)))
       "void hen_main() {\n"
       (apply ++ (generate-registration-calls modules))
       (apply ++ (generate-blott-calls (map c-identifier-safe (snoc modules stub))))
;       (apply ++ (generate-blott-calls (map c-identifier-safe modules)))
       "}")))

;(tracefun gen-main)

(define (output a) `(output ,a))
(define (input a) `(input ,a))
(define (implicit a) `(implicit ,a))

(define (ext f e) (++ f "." e))
(define (exter e) ($ ext _ e))
(define (gco f) `(gcc -std=c99 -g -c -o ,(output (ext f 'o)) ,(input f)))
(define (ss-to-c modules mod) `(,compile-ss-to-c ,modules ,(input (ext mod "stub.ss")) ,(output (ext mod "stub.ss.c")) ,mod))
(define (co f) (ext f 'c.o))
(define (c f) (ext f 'c))
(define (ssco f) (ext f 'ss.c.o))
(define (stub f) (ext f 'stub))
(define (impl f) (ext f 'impl))
(define (stub-ssco f) (ssco (stub f)))
(define (impl-co f) (co (impl f)))
(define (framework l) (++ "-framework " l))

(define (foreign mod)
  (list (ss-to-c '() mod)
        (gco (ext mod "stub.ss.c"))
        (gco (ext mod "impl.c"))))

;; ffis can be:
;;
;;   (ffi "src.c")
;;   (ffi "src.h")
;;   (ffi "<OpenGL/gl.h>")
;;
;; For the .h files, we generate [program]_includer.c that includes
;; those files.
(define ffi-includers '())
(define (gen-include-string import-string)
  (++ "#include " (if (ends-with import-string ">") import-string (++ "\"" import-string "\"")) "\n"))
(define (generate-includer filename import-strings)
  (write-string-to-file filename (apply ++ (map gen-include-string import-strings))))

(define (rigg-rules stub)
  `((rigg ,stub
          (implicit (input ,(ext stub 'c)))
          (implicit (output ,(ext stub 'impl.h)))
          (implicit (output ,(ext stub 'impl.c)))
          (implicit (output ,(ext stub 'stub.ss))))
    (gcc -std=c99 -g -c -o (output ,(ext stub 'impl.c.o)) (input ,(ext stub 'impl.c)))
    (gcc -std=c99 -g -c -o (output ,(ext stub 'c.o)) (input ,(ext stub 'c)))
    (gcc -std=c99 -g -c -o (output ,(ext stub 'stub.ss.c.o)) (input ,(ext stub 'stub.ss.c)))
    (,compile-ss-to-c () (input ,(ext stub 'stub.ss)) (output ,(ext stub 'stub.ss.c)) ,stub)))

(define (implicits . stuff)
  (map implicit (apply append stuff)))
(define (inputs . stuff)
  (map input (apply append stuff)))
(define (outputs . stuff)
  (map output (apply append stuff)))
(define (exts os ex)
  (map ($ ext _ ex) os))

;; Strip <>s
(define (file-base-name name)
  (if (starts-with name "<")
      (substring name 1 (- (string-length name) 1))
      name))

(define (group-imports imports)
  (group-by-preds
   (list
    (lambda (x) (and (eq? (car x) 'ffi)
                     (string=? (get-extension (file-base-name (cadr x))) "h")))
    (lambda (x) (and (eq? (car x) 'ffi)
                     (string=? (get-extension (file-base-name (cadr x))) "c")))
    (lambda (x) (and (eq? (car x) 'link)
                     (string=? (get-extension (file-base-name (cadr x))) "c")))
    (lambda (x) (eq? (car x) 'framework)))
   imports))

(define (build-exe srcfile)
  (let* ((imports (get-imports-from-file srcfile)))
    (mtch (group-imports imports)
          (ffis.h ffis.c libs.c frameworks)
          (let* ((ffis.h (map remove-extension (map file-base-name (map cadr ffis.h))))
                 (ffis.c (map remove-extension (map cadr ffis.c)))
                 (libs.c (map remove-extension (map cadr libs.c)))
                 (runtime '("vor" "mem" "spew" "primcalls"))
                 (framework-string (join-things " " (map-append ($ list "-framework" _) (map cadr frameworks))))
                 (src (remove-extension srcfile))
                 (src.ss (ext src 'ss))
                 (src.ss.c (ext src 'ss.c))
                 (src.ss.c.o (ext src 'ss.c.o))
                 (main (++ src "_main"))
                 (main.c (ext main 'c))
                 (main.c.o (ext main 'c.o))
                 (includer (++ src "_includer"))
                 (link-objs (append `(,main.c.o ,(ext includer 'impl.c.o)) (exts runtime 'c.o)))
                 (includer-generation-rules
                  `((,generate-includer (output ,(ext includer 'c)) ,(exts ffis.h 'h)))) ;,imports)))
                 (includer-rules (rigg-rules includer))
                 (ffis.c-rules (map-append rigg-rules ffis.c))
                 (src-rules
                  `((,compile-ss-to-c () (input ,src.ss) (output ,src.ss.c) ,src)
                    (gcc -std=c99 -g -c -o (output ,src.ss.c.o) (input ,src.ss.c))))
                 (runtime-rules
                  (map gco (exts (append runtime libs.c) 'c)))
                 (main-rules
                  `((,gen-main ,src ,(cons includer ffis.c) (output ,main.c)
                               ,@(implicits (inputs (exts ffis.c 'impl.h))))
                    (gcc -std=c99 -g -c -o (output ,(ext main.c 'o)) (input ,main.c))))
                 (link-rules
                  `((gcc -std=c99 -o (output ,src) (input ,src.ss.c.o) (input ,(ext includer 'stub.ss.c.o)) ,@(inputs link-objs)
                         ,@(inputs (exts ffis.c 'c.o)) ,@(inputs (exts ffis.c 'stub.ss.c.o)) ,@(inputs (exts ffis.c 'impl.c.o))
                         ,@(inputs (exts libs.c 'c.o)) ,framework-string)))
                 (rules (append includer-generation-rules includer-rules ffis.c-rules src-rules runtime-rules main-rules link-rules)))
            ;(shew rules)
            (make src rules)))))
                        
(define (crun-file srcfile run-p delete-p)
  (reset-everything)
  (let* ((exefile (remove-extension srcfile)))
    (build-exe srcfile)
    (if (not (file-exists? exefile))
        (err "No exe.")
        (begin
          (if run-p
              (cmd (++ "./" exefile))
              '())
          (if delete-p
              (rmtemps exefile)
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
   e (simplify (pattern-compile (quote-ctors (doobie (syntax-desugar (expand-do e))))))))

(define global-env '())
(define (clear-global-env) (set! global-env '()))
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
(define (hpair? e) (mtch e ('P 'Cons ('P a ('P d 'Nil))) #t x #f))
(define (hnull? e) (eq? e 'Nil))

(define (high-list->low-list e)
  (cond
   ((hpair? e) (cons (hcar e) (high-list->low-list (hcdr e))))
   ((hnull? e) '())
   (#t (err))))

(define commands '())
(define (register-command name f) (set! commands (cons (cons name f) commands)))

;; HEY rid
(load "shew.impl.ss")

(define (execute-command name args)
  (if show-commands
      (begin
        (display "Command: ")
        (display name)
        (display " ")
        (plshew args)
        (display "\n"))
      '())

  (if (lookup-exists? name commands)
      (apply (lookup name commands) (high-list->low-list args))
      (err "Unknown command" (list name args))))

  ;; (mtch name
;;         'shew (begin (shew (list 'SHEW arg)) 'Nil)
;;         'create-int-ref (create-int-ref arg)
;;         'read-int-ref (read-int-ref arg)
;;         'write-int-ref (write-int-ref arg)
;;         'destroy-int-ref (destroy-int-ref arg)
;;         x (err "Unknown command" (list name arg)))

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

   ('quote s) s

   ('CAR p) (mtch (evl-fully p env) ('P a b) a x (err 'not-pair e))

   ('CDR p) (mtch (evl-fully p env) ('P a b) b x (err 'not-pair e))

   ((('if b) th) el) (mtch (evl-fully b env) 'True (freeze th env) 'False (freeze el env))

   (('+ a) b) (+ (evl-fully a env) (evl-fully b env))

   (('- a) b) (- (evl-fully a env) (evl-fully b env))

   (('* a) b) (* (evl-fully a env) (evl-fully b env))

   (('/ a) b) (* (evl-fully a env) (evl-fully b env))

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
   (('/ a) b) `((/ ,(doobie a)) ,(doobie b))
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
      ((char? x) `(char (char->integer x)))
      ((and (number? x) (inexact? x) (real? x)) `(flote ,x))
      ((and (number? x) (exact? x) (integer? x)) `(integer ,x))
      ((string? x) `(string ,x))
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
        ("interpret" filename) (interpret filename)
        ("compile" filename) (compile filename)
        (filename) (crun filename)))

(define (exp-map f e)
  (mtch
   e

   ('def name val) `(def ,name ,(exp-map f val))
   ('quote x) e
   ('P a b) `(P ,(exp-map f a) ,(exp-map f b))
   ('CAR a) `(CAR ,(exp-map f a))
   ('CDR a) `(CDR ,(exp-map f a))

   (('+ a) b) `((+ ,(exp-map f a)) ,(exp-map f b))
   (('- a) b) `((- ,(exp-map f a)) ,(exp-map f b))
   (('* a) b) `((* ,(exp-map f a)) ,(exp-map f b))
   (('/ a) b) `((/ ,(exp-map f a)) ,(exp-map f b))
   ((('if a) b) c) `(((if ,(exp-map f a)) ,(exp-map f b)) ,(exp-map f c))

   ('/. args body) `(/. ,args ,(exp-map f body))

   ('/./. . lams) `(/./. . ,(map ($ exp-map f _) lams))

   (fn . args) (map-improper ($ exp-map f _) (f e))

   x (f x)))

(define (goulash stuff)
  (mtch stuff
        ('doo v command) `(CommandSeq ,command (/. (,v) ,v))
        ('doo v command . rest) `(CommandSeq ,command (/. (,v) ,(goulash `(doo . ,rest))))))

(define (expand-do-1 e)
  (mtch e
        ('doo .  stuff) (goulash e)
        x x))

(define (expand-do e) (exp-map expand-do-1 e))
;(tracefun preprocess preprocess-program)
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
;(tracefun expand-do expand-do-1 goulash)
;(tracefun goulash)

;; (define rules '(
;;                 (g++ -c (input "hoot.c") (implicit (output "hoot.o")))
;;                 ("g++" "-o" (output "hoot") (input "hoot.o"))
;; ))

;; (make "hoot" rules)
