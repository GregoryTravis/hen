;; (load "h.ss")
(load "lib.ss")

(define match-debug #f)

(define sg (tagged-symbol-generator-generator))

(define (qt s) (list "\"" s "\""))

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
    (#\= . "eq")))

(define (encode-nonalpha-char c)
  (let ((a (assoc c nonalpha-encodings)))
    (if (eq? a #f)
        c
        (cdr a))))

(define (encode-nonalpha s)
  (apply ++ (map encode-nonalpha-char (string->list (->string s)))))

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

(define primitive-function-names
  '((== . eqeq)
    (+ . plus)
    (- . minus)
    (* . times)
    (/ . div)))

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

(define start-symbols '(True False))
(define symbols start-symbols)
(define (add-symbol s) (set! symbols (cons (->symbol s) symbols)))
(define (render-symbol-defs)
  (map render-symbol-def (unique symbols)))
(define (csym s) (begin (add-symbol s) (list "_sym_" (encode-nonalpha s))))
(define (render-symbol-def s)
  (list "static yeah " (csym s) "_ = { TAG_symbol, { .symbol = { " (qt s) " } } };\n"
        "static yeah* " (csym s) " = &" (csym s) "_;\n"))


(define (render-exp b)
  (mtch b
        ('Closure name ('ClosedOverArgs . closed-over-args)) (render-exp `((Sym Closure) ,name . ,closed-over-args))
        (('Sym 'if) b t f) (list "(isbooltrue(" (render-exp b) ", " (csym 'True) ", " (csym 'False) ") ? " "(" (render-exp t) ") : (" (render-exp f) "))")
        ('Sym sym) (csym sym)
        ('Var var) var
        ('Num n) (list "mknumber(" n ")")
        (('Sym a) . d) (if (ctor? a) (render-exp-list b) (render-app-list b))
        (('Var v) . d) (list "apply(" v ", " (render-exp-list d) ")")
        (('Closure name closed-over-args) . args) (list "apply(" (render-exp `(Closure ,name ,closed-over-args)) ", " (render-exp-list args) ")")
        (a . d) (list "apply(" (render-exp a) ", " (render-exp-list d) ")")
        () "mknil()"))

(define (render-exp-list b)
;  (list "mklist" (length b) "(" (join-things ", " b) ")"))
  (mtch b
        (a . d) (list "mkpair(" (render-exp a) ",\n " (render-exp-list d) ")")
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
        ('Num n) (list "mknumber(" n ")")
        (a . d) (list "mkpair(" (render-pat a) ", " (render-pat d) ")")
        () "mknil()"))

(define (group-rules-by-name rules)
  (group-by (lambda (rule) (mtch rule ('Rule (('Sym name) . rest) body) name)) rules))

(define (grouped-rule->C group)
  (let ((name (car group))
        (rule-group (cdr group)))
    `(function ,name (sequence (,(compile-pseudofunction name rule-group)
                                (fail))))))

(define (compile-rules rules)
  (let ((grouped (group-rules-by-name rules)))
    (let ((functions (map grouped-rule->C grouped)))
      (append (map render-declarations functions) (map render functions)))))

(define (render-main start-term)
  (mtch start-term
        (('Sym fun) . rest)
        (list "\n"
            "int main(int argc, char** argv) {\n"
            "  dump(" (render-app-list start-term) ");\n"
            "  return 0;\n";
            "}\n"
            "\n")))

(define (get-fun-names rules)
  (unique
   (map (lambda (rule) (mtch rule ('Rule (('Sym name) . args) body) name)) rules)))

(define (gen-funlies rules)
  (let* ((fun-names (append (map car primitive-function-names) (get-fun-names rules)))
         (fun-cnames (map encode-nonalpha (append (map cdr primitive-function-names) (get-fun-names rules)))))
    (list
     "funly funlies[] = {\n"
     (map (lambda (fun-name fun) (list "  { \"" fun-name "\", &__" fun " },\n")) fun-names fun-cnames)
     "  { NULL, NULL }\n"
     "};\n")))

(define (render-program rules start)
  (let ((compiled-rules (compile-rules rules))
        (funlies (gen-funlies rules)))
    (+++
     (list "#include <stdio.h>\n"
           "#include <stdlib.h>\n"
           "#include \"yeah.h\"\n"
           "#include \"blip.h\"\n"
           "\n"
           (render-symbol-defs)
           "\n"
           (render compiled-rules)
           "\n"
           (render-main start)
           "\n"
           (render funlies)))))

(define (vars-of1 e)
  (mtch e
        ('Sym s) '()
        ('Var v) (list v)
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
   ('Num n) e
   ('MarkedLambda args id vars-to-close-over body) `(Closure (Sym ,id) (ClosedOverArgs . ,vars-to-close-over))
   (a . d) (map lift-marked-rule-exp e)))

(define (lift-gather-additional-rule rule)
  (mtch rule
        ('Rule args body) (lift-gather-additional-exp body)))

(define (lift-gather-additional-exp e)
  (mtch e
   ('Sym s) '()
   ('Var v) '()
   ('Num n) '()
   ('MarkedLambda args id vars-to-close-over body) (append
                                                    (list `(Rule ((Sym ,id) ,vars-to-close-over ,args) ,(lift-marked-rule-exp body)))
                                                    (lift-gather-additional-exp body))
   (a . d) (map-append lift-gather-additional-exp e)))

;(tracefun render-exp)

;(tracefun lift-gather-additional-exp lift-gather-additional-rule lift-marked-rule-exp lift-marked-rule mark-lambda-exp mark-lambda-rule lift-lambdas simplify-program)

(define (parse-exp e is-var)
  (cond
   ((lambda? e) (mtch e ('/. args body)
                      (let ((parsed-args (map ($ parse-exp _ (lambda (v) #t)) args)))
                        `(Lambda ,parsed-args ,(parse-exp body (extend-is-var (vars-of parsed-args) is-var))))))
   ((ctor? e) `(Sym ,e))
   ((quoted-symbol? e) `(Sym ,(cadr e)))
   ((pair? e) (map ($ parse-exp _ is-var) (if (is-var e) (quote-head-function e) e)))
   ((symbol? e) (if (is-var e) `(Var ,e) `(Sym ,e)))
   ((number? e) `(Num ,e))
   (#t (err e))))

(define (fun? f)
  (mtch f
        ('fun args body) #t
        _ #f))

(define (split-program p)
  (mtch (group-by-preds (list fun?) p)
        (funs) `(Parts (Funs ,funs))))

(define (compile-program p)
  (mtch (split-program (syntax-preprocess p))
        (Parts (Funs funs))
        (render-program (simplify-program (map parse-rule funs)) '((Sym main)))))

(define (compile src-stub)
  (let* ((src-file (++ src-stub ".ss"))
         (c-file (++ src-stub ".c"))
         (prog (map-append read-objects (cons src-file autoincludes))))
    (call-with-output-file c-file (lambda (port) (display (compile-program prog) port)))))

(define (ext f e) (++ f "." e))
(define (exter e) ($ ext _ e))
(define (gco f) `(gcc -std=c99 -g -c -o (output ,(ext f 'o)) (input ,(ext f 'c))))

(define (co-exe main modules)
  (let ((os (map (exter "o") (cons main modules))))
    (cons (append `(gcc -g -o (output ,main)) (map (lambda (o) `(input ,o)) os))
          (map gco (cons main modules)))))

(define modules '(yeah spew mem blip yeahlib))

(define autoinclude-rules '((implicit overture.ss)))

(define (make-rules src-stub)
  (append
   `((,compile ,src-stub (implicit (output ,(ext src-stub 'c))) (implicit (input "h.ss")) (implicit (input ,(ext src-stub 'ss))) (implicit (input "yeah.h")) ,@autoinclude-rules)
     ("ctor-gen" "yeah" (implicit (output "yeah.h")) (implicit (output "yeah.c")) (implicit (input "yeah.ctors"))))
   (co-exe src-stub modules)))

(define (build src-stub)
  (make src-stub (make-rules src-stub)))

(define (syntax-preprocess e)
  (list-syntax-preprocess e))

(define (syntax-unpreprocess e)
  (list-syntax-unpreprocess e))

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

;(tracefun render-exp render)
;(tracefun parse simplify-program render-program)
