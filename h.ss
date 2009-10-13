;; (load "h.ss")
(load "lib.ss")

(define match-debug #t)

(define prog
  '((Rule ((Lit foo) (Var a) (Var b)) ((Lit Jerk) (Var b) (Var a)))
    (Rule ((Lit foo) (Var a)) ((Lit Jick) (Var a) (Var a)))
    (Rule ((Lit bar) (Var a) (Var b) (Var cc)) ((Lit foo) (Var cc)))))

(define (compile-pseudofunction name rules)
  `(sequence ,(map compile-rule rules)))

(define (compile-rule rule)
  (mtch rule
        (Rule ((Lit fun) . pat) body) `(match-top ,fun ,pat ,(compile-pat pat body))))

(define (compile-pat pat body)
  (compile-pat-1 'r pat `(build ,body)))

(define (carsym o) (->symbol (++ o 'a)))
(define (cdrsym o) (->symbol (++ o 'd)))

(define (debug-a-match var pat)
  (list "printf(\"- \"); dumps(" (render-data pat) "); printf(\" :: \"); dumps(" var "); printf(\"\\n\");\n"))

(define (debug-wrap var pat code)
  (if match-debug
      `(begin ,(debug-a-match var pat) ,code)
      code))

(define (compile-pat-1 var pat body)
  (debug-wrap
   var
   pat
   (mtch pat
         ('Lit lit) `(if (eq? ,var (Lit ,lit))
                         ,body)

         ('Var a) `(assign ,a ,var ,body)

         () `(if (null? ,var) ,body)

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
      (list "printf(\"(" fun-name " \"); dumps(" (render-data pat) ");\n" "printf(\")\\n\")\n;")
      ""))

(define (render p)
  (mtch p
        ('sequence alts)
        (join-things-list "\n" (map render alts))

        ('if b t)
        (list "if (" (render b) ") {\n" (render t) "\n}")

        ('pair? var)
        (list "ispair(" var ")")

        ('let* assignments body)
        (list (join-things-list "\n" (map render-assignment assignments)) "\n" (render body))

        ('dummy) ";"

        ('assign var exp body) (list (render-assignment `(,var ,exp)) "\n" (render body))

        ('car e) (list (render e) "->u.pair.car")
        ('cdr e) (list (render e) "->u.pair.cdr")
        ('null? e) (list "isnil(" (render e) ")")
        ('eq? a b) (list "samesymbol(" (render a) ", " (render b) ")")

        ('Lit lit) (list "mksymbol(\"" lit "\")")

        ('build b) (list "return " (render-body b) ";")

        ('function name body) (list "yeah* " name "(yeah* r) {\n" (render body) "}\n")

        ('fail) "fprintf(stderr, \"BAD\\n\"); exit(1);\n"

        ('begin a b) (list "{" (render a) (render b) "}")

        ('match-top fun-name pat body) (list (render-match-top fun-name pat) (render body))

        otherwise p))

(define (ctor-lit? a)
  (mtch a
        ('Lit a) (ctor? a)))

(define (render-body b)
  (mtch b
        ('Lit sym) (list "mksymbol(\"" sym "\")")
        ('Var var) var
        ;(a . d) (list "mkpair(" (render-body a) ", " (render-body d) ")")
        (a . d) (if (ctor-lit? a) (render-body-list b) (render-app-list b))
        () "mknil()"))

(define (render-body-list b)
  (mtch b
        (a . d) (list "mkpair(" (render-body a) ", " (render-body-list d) ")")
        () "mknil()"))

(define (render-app-list b)
  (mtch b
        ((Lit a) . d) (list a "(" (render-body-list d) ")")))

(define (render-data b)
  (mtch b
        ('Lit sym) (list "mksymbol(\"" sym "\")")
        ('Var var) (list "mksymbol(\"" var "\")")
        (a . d) (list "mkpair(" (render-data a) ", " (render-data d) ")")
        () "mknil()"))

(define (compile-rules rules)
  (let ((grouped (group-by (lambda (rule) (mtch rule ('Rule (('Lit name) . rest) body) name)) rules)))
    (map render
         (map (lambda (group)
                (let ((name (car group))
                      (rule-group (cdr group)))
                  `(function ,name (sequence (,(compile-pseudofunction name rule-group)
                                              (fail))))))
              grouped))))

(define (render-main start-term)
  (mtch start-term
        (('Lit fun) . rest)
        (list "\n"
            "int main(int argc, char** argv) {\n"
            "  dump(" fun  "(" (render-data rest) "));\n"
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

;(tracefun +++ ++)

(define start-term '((Lit foo) (Lit but) (Lit hut)))
(define start-term '((Lit foo) (Lit but)))
(define start-term '((Lit bar) (Lit aaaa) (Lit bbbb) (Lit cccc)))

(call-with-output-file "hoop.c" (lambda (port) (display (render-program prog start-term) port)))
