;; (load "h.ss")
(load "lib.ss")

(define match-debug #f)
(define atexit #f)

(define sg (tagged-symbol-generator-generator))

(define (qt s) (list "\"" s "\""))

(define primitive-function-names
  '((== . eqeq)
    (+ . plus)
    (- . minus)
    (* . times)
    (/ . div)))

(define autoincludes '("overture.ss"))

(define (compile-pseudofunction name rules)
  `(sequence ,(map compile-rule rules)))

(define (compile-rule rule)
  (mtch rule
        (Rule ((Sym fun) . pat) body) `(match-top ,fun ,pat ,(compile-pat pat body))))

(define (compile-pat pat body)
  (compile-pat-1 'r pat `(build ,body)))

(define (carsym o) (->symbol (++ o 'a)))
(define (cdrsym o) (->symbol (++ o 'd)))

(define nonalpha-encodings
  '((#\+ . "plus")
    (#\- . "minus")
    (#\* . "times")
    (#\/ . "div")
    (#\= . "eq")
    (#\$ . "buck")
    (#\. . "dot")))

(define (encode-nonalpha-char c)
  (let ((a (assoc c nonalpha-encodings)))
    (if (eq? a #f)
        c
        (cdr a))))

(define (encode-nonalpha s)
  (->symbol (apply ++ (map encode-nonalpha-char (string->list (->string s))))))


(define (debug-a-match var pat)
  (list "printf(\"- \"); dumps(" (render-pat pat) "); printf(\" :: \"); dump(" var ");\n"))

(define (debug-wrap var pat code)
  (if match-debug
      `(begin ,(debug-a-match var pat) ,code)
      code))

(define (compile-pat-1 var pat body)
  (debug-wrap
   var
   pat
   (mtch pat
         ('Sym lit) `(if (eq? ,var ,pat) ,body)
         ('Num n) `(if (eq? ,var ,pat) ,body)
         () `(if (eq? ,var ,pat) ,body)

         ('Var a) `(assign ,a ,var ,body)

         (a . d)
         (let ((var-a (carsym var))
               (var-d (cdrsym var)))
           `(if (pair? ,var)
                (let* ((,var-a (car ,var))
                       (,var-d (cdr ,var)))
                  ,(compile-pat-1 var-a a (compile-pat-1 var-d d body))))))))

(define (render-assignment ass)
  (mtch ass
        (var exp)
        (list "yeah* " var " = " (render exp) ";")))

(define (render-match-top fun-name pat)
  (if match-debug
      (list "printf(\"* (" fun-name " \"); dumps(" (render-pat pat) "); printf(\")\\n\");")
      ""))

(define (render p)
  (mtch p
        ('sequence alts)
        (join-things-list "\n" (map render alts))

        ('if b t)
        (list "if (" (render b) ") {\n"
              (if match-debug "printf(\"  - success: !\\n\");\n" "")
              (render t) "\n}"
              (if match-debug (list " else { printf(\"  - fail: line %d %s !\\n\", __LINE__, \"" b "\"); }\n") ""))

        ('pair? var)
        (list "ispair(" var ")")

        ('let* assignments body)
        (list (join-things-list "\n" (map render-assignment assignments)) "\n" (render body))

        ('dummy) ";"

        ('assign var exp body) (list (render-assignment `(,var ,exp)) "\n" (render body))

        ('car e) (list (render e) "->u.pair.car")
        ('cdr e) (list (render e) "->u.pair.cdr")

        ; TODO: since we know the type of the pat, this should use a specialized equality routine.
        ('eq? a b) (list "eq(" (render a) ", " (render b) ")")

        ('Sym lit) (render-exp p)
        ('Num n) (render-exp p)
        () (render-exp p)

        ('build b) (list "return " (render-exp b) ";")

        ('function name body) (list "yeah* __" (encode-nonalpha name) "(yeah* r) {\n" (render body) "}\n")

        ('fail) "fprintf(stderr, \"BAD\\n\"); exit(1);\n"

        ('begin a b) (list "{" (render a) (render b) "}")

        ('match-top fun-name pat body) (list (render-match-top fun-name pat) (render body))

        otherwise p))

(define (render-declarations p)
  (mtch p
        ('function name body) (list "yeah* __" (encode-nonalpha name) "(yeah* r);\n")))

(define start-objects
  (append
   '((symbol True) (symbol False) (symbol $) (symbol Closure) (symbol Command) (function prim_putchar) (function prim_getchar) (symbol Cons) (symbol Nil))
   (map (lambda (primfun) `(function ,primfun)) (map cdr primitive-function-names))))
(define objects start-objects)

(define (name-c-global x) (list '_blargh_ x))

(define (add-object tag o) (set! objects (cons `(,tag ,o) objects)))
(define (cobj tag o) (begin (add-object tag o) (name-c-global (encode-nonalpha o))))

(define (csym s) (cobj 'symbol (encode-nonalpha s)))
(define (cnum n) (cobj 'number n))
(define (cfunction f) (cobj 'function f))

(define (gather-globals tlfs)
  (map gather-global tlfs))

(define (gather-global tlf)
  (mtch tlf
        ('Rule (('Sym name) . rest) body) (cfunction name)))
;        ('def name x) (if (number? x) (cnum x) (csym x))))

(define global-refs '())

(define (gather-global-refs tlfs)
  (set! global-refs (append global-refs (unique (map-append gather-global-refs-tlf tlfs)))))

(define (gather-global-refs-tlf tlf)
  (mtch tlf
        ('Rule pat body) (gather-global-refs-exp body)))

(define (gather-global-refs-exp body)
  (mtch body
        ('GVar 'if) '()
        ('GVar gvar) (list gvar)
        ('Num n) '()
        ('Sym s) '()
        ('Closure name env) '()
        ('Var v) '()
        (a . d) (map-append gather-global-refs-exp body)))

(define ffi-info #f)

(define (gather-ffi-info ffi-decls)
  (set! ffi-info (map-append (lambda (d) (mtch d ('ffi stub) (read-and-maybe-generate-ffi-info stub))) ffi-decls)))
(define (read-and-maybe-generate-ffi-info stub)
  (let ((blick (++ stub ".blick")))
    (if (not (file-exists? blick))
        (cmd (++ "rigg " stub))
        '())
    (car (read-objects blick))))

;(define (find-ffi name) (lookup name ffi-info))

(define (get-undefined-globals)
  (set-difference global-refs (map cadr objects)))

(define (generate-foreign-stub-declarations)
  (map generate-foreign-stub-declaration (get-undefined-globals)))

(define (generate-foreign-stub-declaration name)
;  (list "yeah* __" name "(yeah* args);\n"))
  (list (generate-foreign-stub-declaration-1 name) ";\n"))

(define (generate-foreign-stubs)
  (map generate-foreign-stub (get-undefined-globals)))

(define (generate-foreign-stub-declaration-1 name)
  (list "yeah* __" name "(yeah* args)"))
;;   (let ((info (lookup name ffi-info)))
;;     (list (lookup 'return_type info) __ name "("
;;           (lookup 'params info) ")")))

(define (generate-foreign-stub name)
  (list (generate-foreign-stub-declaration-1 name) "{\n"
        (generate-foreign-stub-body name)
        "}\n\n"))

(define (generate-foreign-stub-body name)
  (let* ((info (lookup name ffi-info))
         (params (lookup 'params info))
         (return_type (lookup 'return_type info)))
    (list "  yeah* here = args;\n"
          (map (lambda (param)
                 (mtch param
                       (type param-name)
                       (list "  A(ispair(here));\n"
                             (param-extractor param-name type "car(here)")
                             "  here = cdr(here);\n")))
               params)
          "  A(isnil(here));\n"
          "  " (if (== (->string return_type) "void") "" (list return_type " __ret = ")) name "(" (join-things ", " (map cadr params)) ");\n"
          "  return " (param-builder return_type "__ret") ";\n")))

(define (param-extractor name type e)
  (mtch (->symbol type)
        'float* (list "  A(isopaque(" e "));\n" "  float* " name " = (float*)"e "->u.opaque.o;\n")
        'float (list "  A(isnumber(" e "));\n" "  float " name " = " e "->u.number.d;\n")
        'void* (list "  A(isopaque(" e "));\n" "  float* " name " = (void*)"e "->u.opaque.o;\n")
        'void (list "  A(isnil(" e "));\n" "  float* " name " = mknil()")))

(define (param-builder type e)
  (mtch (->symbol type)
        'float* (list "mkopaque((void*)" e ")")
        'float (list "mknumber(" e ")")
        'void* (list "mkopaque((void*)" e ")")
        'void (list "mknil()")))

(define (ffi-includes decls)
  (map (lambda (stub) (list "#include \"" stub ".h\"\n")) (map cadr decls)))

(define (render-object-defs)
  (map render-object-def (unique objects)))

(define (render-object-def o)
  (mtch o
        ('symbol s) (list "yeah " (csym s) "_ = { TAG_symbol, { .symbol = { " (qt s) " } } };\n"
                          "yeah* " (csym s) " = &" (csym s) "_;\n")
        ('number n) (list "yeah " (cnum n) "_ = { TAG_number, { .number = { " n " } } };\n"
                          "yeah* " (cnum n) " = &" (cnum n) "_;\n")
        ('function f) (list "yeah " (cfunction f) "_ = { TAG_function, { .function = { &__" f ", \"" f "\" } } };\n"
                            "yeah* " (cfunction f) " = &" (cfunction f) "_;\n")))

(define (render-exp b)
  (mtch b
        ('Closure name ('ClosedOverArgs . closed-over-args)) (render-exp `((Sym Closure) ,name . ,closed-over-args))
        (('GVar 'if) b t f) (list "(isbooltrue(" (render-exp b) ", " (csym 'True) ", " (csym 'False) ") ? " "(" (render-exp t) ") : (" (render-exp f) "))")
        ('Sym sym) (csym sym)
        ('Var var) var
        ('GVar var) (name-c-global var)
        ('Num n) (list (cnum n))
        (('Sym a) . d) (if (ctor? a) (render-exp-list b) (render-app-list b))
        (('Var v) . d) (list "apply(" v ", " (render-exp-list d) ")")
        (('Closure name closed-over-args) . args) (list "apply(" (render-exp `(Closure ,name ,closed-over-args)) ", " (render-exp-list args) ")")
        (('GVar name) . d) (list "__" name "(" (render-exp-list d) ")")
        (a . d) (list "apply(" (render-exp a) ", " (render-exp-list d) ")")
        () "mknil()"))

(define (render-exp-list b)
  (mtch b
        (a . d) (list "mkpair(\n" (render-exp a) ",\n " (render-exp-list d) ")")
        () "mknil()"))

(define (rename-fun-maybe name)
  (translate-or-not name primitive-function-names))

(define (render-app-list b)
  (mtch b
        ((Sym a) . d) (list "__" (encode-nonalpha (rename-fun-maybe a)) "(" (render-exp-list d) ")")))

(define (render-pat b)
  (mtch b
        ('Sym sym) (list (csym sym))
        ('Var var) (list (csym var))
        ('Num n) (list (cnum n))
        (a . d) (list "mkpair(\n" (render-pat a) ",\n" (render-pat d) ")")
        () "mknil()"))

(define (group-rules-by-name rules)
  (group-by (lambda (rule) (mtch rule ('Rule (('Sym name) . rest) body) name)) rules))

(define (grouped-rule->C group)
  (let ((name (car group))
        (rule-group (cdr group)))
    `(function ,name (sequence (,(compile-pseudofunction name rule-group)
                                (fail))))))

(define (rules->c-functions rules)
  (let ((grouped (group-rules-by-name rules)))
    (map grouped-rule->C grouped)))

(define (render-main start-term)
  (mtch start-term
        (('Sym fun) . rest)
        (list "\n"
            "int main(int argc, char** argv) {\n"
            (if atexit "  atexit(&yeah_atexit);\n" "")
            "  dump(driver(" (render-app-list start-term) "));\n"
            "  return 0;\n";
            "}\n"
            "\n")))

(define (get-fun-names rules)
  (unique
   (map (lambda (rule) (mtch rule ('Rule (('Sym name) . args) body) name)) rules)))

(define (render-program rules ffi-decls start)
  (let* ((c-functions (rules->c-functions rules))
         (rendered-functions (map render c-functions))
         (rendered-declarations (map render-declarations c-functions))
         (foreign-declarations (generate-foreign-stub-declarations))
         (foreign-stubs (generate-foreign-stubs)))
    (+++
     (list "#include <stdio.h>\n"
           "#include <stdlib.h>\n"
           "#include \"yeah.h\"\n"
           "#include \"yeahlib.h\"\n"
           "#include \"mem.h\"\n"
           "#include \"blip.h\"\n"
           (ffi-includes ffi-decls)
           "\n"
           rendered-declarations
           "\n"
           foreign-declarations
           "\n"
           (render-object-defs)
           "\n"
           rendered-functions
           "\n"
           foreign-stubs
           "\n"
           (render-main start)))))

(define (vars-of1 e)
  (mtch e
        ('Sym s) '()
        ('Var v) (list v)
        ('GVar v) '()
        ('Num n) '()
        (a . d) (map-append vars-of1 e)
        () '()))

(define (vars-of e) (unique (vars-of1 e)))

(define (extend-is-var more-vars is-var) (lambda (v) (or (member? v more-vars) (is-var v))))

(define (parse-rule rule)
  (mtch rule
        ('fun pat body)
        (let* ((parsed-pat (parse-exp pat (lambda (v) #t)))
               (bound-vars (vars-of parsed-pat))
               (parsed-body (parse-exp body (lambda (v) (member? v bound-vars)))))
          `(Rule ,parsed-pat ,parsed-body))))

(define (quote-head-function e)
  (cons (if (symbol? (car e)) `(quote ,(car e)) (car e)) (cdr e)))

(define (simplify-program prog)
  (lift-lambdas prog))

(define (lift-lambdas rules)
  (let* ((marked (map mark-lambda-rule rules))
         (lifted-rules (map lift-marked-rule marked))
         (additional-rules (map-append lift-gather-additional-rule marked)))
    (append additional-rules lifted-rules)))

(define (mark-lambda-rule rule)
  (mtch rule
        ('Rule args body) `(Rule ,args ,(mark-lambda-exp body (vars-of args)))))

(define (mark-lambda-exp e bound-vars)
  (mtch e
   ('Sym s) e
   ('Var v) e
   ('GVar v) e
   ('Num n) e
   ('Lambda args body) (let* ((lift-id (sg 'lambda_))
                              (lifted-body (mark-lambda-exp body (append bound-vars (vars-of args))))
                              (vars-to-close-over (map (lambda (x) `(Var ,x)) bound-vars)))
                                                       ;(set-difference (vars-of lifted-body) (vars-of args)))))
                         `(MarkedLambda ,args ,lift-id ,vars-to-close-over ,lifted-body))
   (a . d) (map ($ mark-lambda-exp _ bound-vars) e)))

(define (lift-marked-rule rule)
  (mtch rule
        ('Rule args body) `(Rule ,args ,(lift-marked-rule-exp body))))

(define (lift-marked-rule-exp e)
  (mtch e
   ('Sym s) e
   ('Var v) e
   ('GVar v) e
   ('Num n) e
   ('MarkedLambda args id vars-to-close-over body) `(Closure (GVar ,id) (ClosedOverArgs . ,vars-to-close-over))
   (a . d) (map lift-marked-rule-exp e)))

(define (lift-gather-additional-rule rule)
  (mtch rule
        ('Rule args body) (lift-gather-additional-exp body)))

(define (lift-gather-additional-exp e)
  (mtch e
   ('Sym s) '()
   ('Var v) '()
   ('GVar v) '()
   ('Num n) '()
   ('MarkedLambda args id vars-to-close-over body) (append
                                                    (list `(Rule ((Sym ,id) ,vars-to-close-over ,args) ,(lift-marked-rule-exp body)))
                                                    (lift-gather-additional-exp body))
   (a . d) (map-append lift-gather-additional-exp e)))

;(tracefun lift-gather-additional-exp lift-gather-additional-rule lift-marked-rule-exp lift-marked-rule mark-lambda-exp mark-lambda-rule lift-lambdas simplify-program)

(define (divdot? e) (mtch e ('divdot . rest) #t _ #f))

(define (parse-exp e is-var)
  (cond
   ((divdot? e) (mtch e ('divdot args body)
                      (let ((parsed-args (map ($ parse-exp _ (lambda (v) #t)) args)))
                        `(Lambda ,parsed-args ,(parse-exp body (extend-is-var (vars-of parsed-args) is-var))))))
   ((ctor? e) `(Sym ,e))
   ((quoted-symbol? e) `(Sym ,(cadr e)))
   ((pair? e) (map ($ parse-exp _ is-var) (if (is-var e) (quote-head-function e) e)))
   ((symbol? e) (if (is-var e) `(Var ,e) `(GVar ,e)))
   ((number? e) `(Num ,e))
;   ((null? e) e)
   (#t (err 'parse-exp e))))

(define (fun? f)
  (mtch f
        ('fun args body) #t
        _ #f))

(define (ffi-decl? f)
  (mtch f
        ('ffi stub) #t
        _ #f))

(define (split-program p)
  (mtch (group-by-preds (list fun? ffi-decl?) p)
        (funs ffi-decls) `(Parts (Funs ,funs) (FFIDecls ,ffi-decls))))

(define (compile-program p)
  (let ((preprocessed (syntax-preprocess p)))
    (mtch (split-program preprocessed)
          ('Parts ('Funs funs) ('FFIDecls ffi-decls))
          (let ((simplified (simplify-program (map parse-rule funs))))
            (gather-ffi-info ffi-decls)
            (gather-globals simplified)
            (gather-global-refs simplified)
            ;(shew objects global-refs (get-undefined-globals))
            (render-program simplified ffi-decls '((Sym main)))))))

(define (compile src-stub)
  (let* ((src-file (++ src-stub ".ss"))
         (c-file (++ src-stub ".c"))
         (prog (map-append read-objects (cons src-file autoincludes))))
    (call-with-output-file c-file (lambda (port) (display (compile-program prog) port)))))

(define gcc-options "-g -O6 -Wall -std=c99 -Wno-unused-variable")
(define (ext f e) (++ f "." e))
(define (exter e) ($ ext _ e))
(define (gco f) `(gcc ,gcc-options -c -o (output ,(ext f 'o)) (input ,(ext f 'c))))

(define (co-exe main modules)
  (let ((os (map (exter "o") (cons main modules))))
    (cons (append `(gcc ,gcc-options -o (output ,main)) (map (lambda (o) `(input ,o)) os))
          (map gco (cons main modules)))))

(define modules '(yeah spew mem blip yeahlib ref shew))

(define autoinclude-rules (map (lambda (file) `(implicit ,file)) autoincludes))

(define (make-rules src-stub)
  (append
   `((,compile ,src-stub (implicit (output ,(ext src-stub 'c))) (implicit (input "h.ss")) (implicit (input ,(ext src-stub 'ss))) (implicit (input "yeah.h")) ,@autoinclude-rules)
     ("ctor-gen" "yeah" (implicit (output "yeah.h")) (implicit (output "yeah.c")) (implicit (input "yeah.ctors"))))
   (co-exe src-stub modules)))

(define (build src)
  (let ((src-stub (remove-extension src)))
    (make src-stub (make-rules src-stub))))

(define (syntax-preprocess e)
  (operator-rename-preprocess (doo-preprocess (list-syntax-preprocess e))))

(define (syntax-unpreprocess e)
  (operator-rename-unpreprocess (list-syntax-unpreprocess e)))

(define (operator-rename-preprocess e)
  (mtch e
        (a . d) (cons (operator-rename-preprocess a) (operator-rename-preprocess d))
        e (if (symbol? e) (encode-nonalpha e) e)))

(define (operator-rename-unpreprocess e) e)
;(tracefun syntax-preprocess)

(define (list-syntax-preprocess e)
  (mtch e
        ('$ . rest) (list-syntax-preprocess-list rest)
        (a . d) (map list-syntax-preprocess e)
        x x))

(define (list-syntax-preprocess-list e)
  (cond
   ((pair? e) `(Cons ,(list-syntax-preprocess (car e)) ,(list-syntax-preprocess-list (cdr e))))
   ((null? e) 'Nil)
   (#t e)))

(define (list-syntax-unpreprocess e)
  (mtch e
        ('Cons a b) (list-syntax-unpreprocess-list e)
        'Nil (list-syntax-unpreprocess-list e)
        (a . d) (map list-syntax-unpreprocess-list e)
        x x))

(define (list-syntax-unpreprocess-list e)
  `($ . ,(list-syntax-unpreprocess-list1 e)))
(define (list-syntax-unpreprocess-list1 e)
  (mtch e
        ('Cons a d) (cons (list-syntax-unpreprocess a) (list-syntax-unpreprocess-list1 d))
        'Nil '()
        x x))

(define (doo-preprocess e)
  (mtch e
        ('doo . doos) (rewrite-doos doos)
        (a . d) (map doo-preprocess e)
        e e))

(define (rewrite-doos doos)
  (mtch doos
        (var (command-function . args) . rest) `(Command (/. () (,command-function . ,args)) (/. (,var) ,(rewrite-doos rest)))
        '() 45))

;(tracefun render-exp render render-object-def)
;(tracefun render-pat csym)
;(tracefun parse-rule parse-exp simplify-program)
;(tracefun syntax-preprocess syntax-unpreprocess)
;(tracefun list-syntax-unpreprocess-list1 list-syntax-unpreprocess list-syntax-unpreprocess-list list-syntax-preprocess-list list-syntax-preprocess)
;(tracefun operator-rename-unpreprocess operator-rename-preprocess)
;(tracefun list-syntax-preprocess)

(define (skiff stub)
  (mtch (read-objects (++ stub ".blick"))
        ((src stub funs))
        (let ((ffuns (map (lambda (props) (list (lookup 'name props) props)) funs)))
          (shew ffuns))))
