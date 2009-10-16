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
  '((== . eqeq)))

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
        ('Sym sym) (list "mksymbol(\"" sym "\")")
        ('Var var) var
        ('Num n) (list "mknumber(" n ")")
        ;(a . d) (list "mkpair(" (render-exp a) ", " (render-exp d) ")")
        (a . d) (if (ctor-lit? a) (render-exp-list b) (render-app-list b))
        () "mknil()"))

(define (render-exp-list b)
  (mtch b
        (a . d) (list "mkpair(" (render-exp a) ", " (render-exp-list d) ")")
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

(define (render-program rules start)
  (+++
   (list "#include <stdio.h>\n"
         "#include <stdlib.h>\n"
         "#include \"yeah.h\"\n"
         "#include \"blip.h\"\n"
         (render (compile-rules rules))
         (render-main start))))

(define (parse src)
  (map parse-rule src))

(define (parse-rule rule)
  (mtch rule
        ('fun pat body)
        `(Rule ,(parse-exp pat) ,(parse-exp body))))

(define (quote-head-function e)
  (cons (if (symbol? (car e)) `(quote ,(car e)) (car e)) (cdr e)))

(define (parse-exp e)
  (cond
   ((ctor? e) `(Sym ,e))
   ((quoted-symbol? e) `(Sym ,(cadr e)))
   ((pair? e) (map parse-exp (quote-head-function e)))
   ((symbol? e) `(Var ,e))
   ((number? e) `(Num ,e))
   (#t (err e))))
;(tracefun parse-exp)

(define (run-file src-file)
  (let ((prog (read-objects src-file)))
    (call-with-output-file "hoop.c" (lambda (port) (display (render-program (parse prog) '((Sym main))) port)))))
