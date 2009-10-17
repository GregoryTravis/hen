;; (load "h.ss")
(load "lib.ss")

(define match-debug #f)

(define (compile-pseudofunction name rules)
  `(sequence ,(map compile-rule rules)))

(define (compile-rule rule)
  (mtch rule
        (Rule ((Sym fun) . pat) body) `(match-top ,fun ,pat ,(compile-pat pat body))))

(define (compile-pat pat body)
  (compile-pat-1 'r pat `(build ,body)))

(define (carsym o) (->symbol (++ o 'a)))
(define (cdrsym o) (->symbol (++ o 'd)))

(define (debug-a-match var pat)
  (list "printf(\"- \"); dumps(" (render-pat pat) "); printf(\" :: \"); dumps(" var "); printf(\"\\n\");\n"))

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
      (list "printf(\"(" fun-name " \"); dumps(" (render-pat pat) ");\n" "printf(\")\\n\")\n;")
      ""))

(define (render p)
  (mtch p
        ('sequence alts)
        (join-things-list "\n" (map render alts))

        ('if b t)
        (list "if (" (render b) ") {\n"
              (if match-debug "printf(\"  - success!\\n\");\n" "")
              (render t) "\n}" (if match-debug " else { printf(\"  - fail!\\n\"); }" ""))

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

        ('function name body) (list "yeah* __" name "(yeah* r) {\n" (render body) "}\n")

        ('fail) "fprintf(stderr, \"BAD\\n\"); exit(1);\n"

        ('begin a b) (list "{" (render a) (render b) "}")

        ('match-top fun-name pat body) (list (render-match-top fun-name pat) (render body))

        otherwise p))

(define (ctor-lit? a)
  (mtch a
        ('Sym a) (ctor? a)))

(define (render-exp b)
  (mtch b
        (('Sym 'if) b t f) (list "(eq(" (render-exp b) ", mksymbol(\"True\")) ? " "(" (render-exp t) ") : (" (render-exp f) "))")
        ('Sym sym) (list "mksymbol(\"" sym "\")")
        ('Var var) var
        ('Num n) (list "mknumber(" n ")")
        ;(a . d) (list "mkpair(" (render-exp a) ", " (render-exp d) ")")
        ;(a . d) (if (ctor-lit? a) (render-exp-list b) (render-app-list b))
        (('Sym a) . d) (if (ctor? a) (render-exp-list b) (render-app-list b))
        (('Var v) . d) (list "(funlookup(" v "))(" (render-exp-list d) ")")
        () "mknil()"))

(define (render-exp-list b)
  (mtch b
        (a . d) (list "mkpair(" (render-exp a) ",\n " (render-exp-list d) ")")
        () "mknil()"))

(define (rename-fun-maybe name)
  (translate-or-not name primitive-function-names))

(define (render-app-list b)
  (mtch b
        ((Sym a) . d) (list "__" (rename-fun-maybe a) "(" (render-exp-list d) ")")))

(define (render-pat b)
  (mtch b
        ('Sym sym) (list "mksymbol(\"" sym "\")")
        ('Var var) (list "mksymbol(\"" var "\")")
        ('Num n) (list "mknumber(" n ")")
        (a . d) (list "mkpair(" (render-pat a) ", " (render-pat d) ")")
        () "mknil()"))

(define (compile-rules rules)
  (let ((grouped (group-by (lambda (rule) (mtch rule ('Rule (('Sym name) . rest) body) name)) rules)))
    (map render
         (map (lambda (group)
                (let ((name (car group))
                      (rule-group (cdr group)))
                  `(function ,name (sequence (,(compile-pseudofunction name rule-group)
                                              (fail))))))
              grouped))))

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
  (let ((fun-names (get-fun-names rules)))
    (list
     "funly funlies[] = {\n"
     (map (lambda (fun-name) (list "  { \"" fun-name "\", &__" fun-name " },\n")) fun-names)
     "  { NULL, NULL }\n"
     "};\n")))

(define (render-program rules start)
  (+++
   (list "#include <stdio.h>\n"
         "#include <stdlib.h>\n"
         "#include \"yeah.h\"\n"
         "#include \"blip.h\"\n"
         (render (compile-rules rules))
         (render (gen-funlies rules))
         (render-main start))))

(define (parse src)
  (map parse-rule src))

(define (vars-of e)
  (mtch e
        ('Sym s) '()
        ('Var v) (list v)
        ('Num n) '()
        (a . d) (map-append vars-of e)))

(define (parse-rule rule)
  (mtch rule
        ('fun pat body)
        (let* ((parsed-pat (parse-exp pat (lambda (v) #t)))
               (bound-vars (vars-of parsed-pat))
               (parsed-body (parse-exp body (lambda (v) (member? v bound-vars)))))
          `(Rule ,parsed-pat ,parsed-body))))

(define (quote-head-function e)
  (cons (if (symbol? (car e)) `(quote ,(car e)) (car e)) (cdr e)))

(define (parse-exp e is-var)
  (cond
   ((ctor? e) `(Sym ,e))
   ((quoted-symbol? e) `(Sym ,(cadr e)))
   ((pair? e) (map ($ parse-exp _ is-var) (if (is-var e) (quote-head-function e) e)))
   ((symbol? e) (if (is-var e) `(Var ,e) `(Sym ,e)))
   ((number? e) `(Num ,e))
   (#t (err e))))
;(tracefun parse-exp vars-of)

(define (compile src-stub)
  (let* ((src-file (++ src-stub ".ss"))
         (c-file (++ src-stub ".c"))
         (prog (read-objects src-file)))
    (call-with-output-file c-file (lambda (port) (display (render-program (parse prog) '((Sym main))) port)))))

(define (ext f e) (++ f "." e))
(define (exter e) ($ ext _ e))
(define (gco f) `(gcc -std=c99 -g -c -o (output ,(ext f 'o)) (input ,(ext f 'c))))

(define (co-exe main modules)
  (let ((os (map (exter "o") (cons main modules))))
    (cons (append `(gcc -g -o (output ,main)) (map (lambda (o) `(input ,o)) os))
          (map gco (cons main modules)))))

(define modules '(yeah spew mem blip yeahlib))

(define (build src-stub)
  (make src-stub
    (append
     `((,compile ,src-stub (implicit (output ,(ext src-stub 'c))) (implicit (input "h.ss")) (implicit (input ,(ext src-stub 'ss))) (implicit (input "yeah.h")))
       ("ctor-gen" "yeah" (implicit (output "yeah.h")) (implicit (output "yeah.c")) (implicit (input "yeah.ctors"))))
     (co-exe src-stub modules))))
