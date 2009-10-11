;; (load "h.ss")
(load "lib.ss")

(define prog
  '((Rule ((Lit foo) (Var a) (Var b)) ((Lit Jerk) (Var b) (Var a)))
    (Rule ((Lit foo) (Var a)) ((Lit Jick) (Var a) (Var a)))
    (Rule ((Lit bar) (Var a) (Var b) (Var c)) ((Lit pott) (Var c)))))

(define (compile-pseudofunction rules)
  ;(join-things "\n\n" (map compile-rule rules)))
  `(sequence ,(map compile-rule rules)))

(define (compile-rule rule)
  (mtch rule
        (Rule pat body) (compile-pat pat body)))

(define (compile-pat pat body)
  (compile-pat-1 'r pat `(build ,body)))

(define (carsym o) (->symbol (++ o 'a)))
(define (cdrsym o) (->symbol (++ o 'd)))

(define (compile-pat-1 var pat body)
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
                      ,(compile-pat-1 var-a a (compile-pat-1 var-d d body)))))))

(define (render-assignment ass)
  (mtch ass
        (var exp)
        (++ "yeah* " var " = " (render exp) ";")))

(define (render p)
  (mtch p
        ('sequence alts)
        (join-things "\n" (map render alts))

        ('if b t)
        (++ "if (" (render b) ") {\n" (render t) "\n}")

        ('pair? var)
        (++ "ispair(" var ")")

        ('let* assignments body)
        (++ (join-things "\n" (map render-assignment assignments)) "\n" (render body))

        ('dummy) ";"

        ('assign var exp body) (++ (render-assignment `(,var ,exp)) "\n" (render body))

        ('car e) (++ (render e) "->u.pair.car")
        ('cdr e) (++ (render e) "->u.pair.cdr")
        ('null? e) (++ "isnil(" (render e) ")")
        ('eq? a b) (++ (render a) " == " (render b))

        ('Lit lit) (++ "mksymbol(\"" lit "\")")

        ('build b) (++ "return " (render-data b) ";")

        ('function name body) (++ "yeah* " name "(yeah* r) {\n" (render body) "}\n")

        ('fail) "fprintf(stderr, \"BAD\\n\"); exit(1);\n"

        otherwise p))

(define (render-data b)
  (mtch b
        ('Lit sym) (++ "mksymbol(\"" sym "\")")
        ('Var var) var
        (a . d) (++ "mkpair(" (render-data a) ", " (render-data d) ")")
        () "mknil()"))

(define (compile-rules rules)
  (let ((grouped (group-by (lambda (rule) (mtch rule ('Rule (('Lit name) . rest) body) name)) rules)))
    (apply ++
           (map render
                (map (lambda (group)
                       (let ((name (car group))
                             (rule-group (cdr group)))
                         `(function ,name (sequence (,(compile-pseudofunction rule-group)
                                                     (fail))))))
                     grouped)))))

(define (render-main start-term)
  (mtch start-term
        (('Lit fun) . rest)
        (++ "\n"
            "int main(int argc, char** argv) {\n"
            "  foo(" (render-data start-term) ");\n"
            "}\n"
            "\n")))

(define (render-program rules start)
  (++ "#include <stdio.h>\n"
      "#include <stdlib.h>\n"
      "#include \"yeah.h\"\n"
      (render (compile-rules rules))
      (render-main start)))

(define start-term '((Lit foo) (Lit but) (Lit hut)))

(display (render-program prog start-term))
