;; (load "h.ss")
(load "lib.ss")

(define prog
  '((Rule ((Lit foo) (Var a) (Var b)) ((Lit Jerk) (Var b) (Var a)))
    (Rule ((Lit foo) (Var a)) ((Lit Jick) (Var a) (Var a)))))

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

;(tracefun compile-pat-1)

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

        ('build b) (++ "return " (render-build b) ";")

        ('function name body) (++ "yeah* " name "(yeah* r) {\n" (render body) "}\n")

        ('fail) "fprintf(stderr, \"BAD\\n\"); exit(1);\n"

        otherwise p))

(define (render-build b)
  (mtch b
        ('Lit sym) (++ "mksymbol(\"" sym "\")")
        ('Var var) var
        (a . d) (++ "mkpair(" (render-build a) ", " (render-build d) ")")
        () "mknil()"))

;(tracefun render)

(define (compile-rules rules)
  (let ((name (mtch rules (('Rule (('Lit name) . rest) body) . rest2) name)))
    `(function ,name (sequence (,(compile-pseudofunction rules)
                                (fail))))))

(define (render-program rules)
  (++ "#include <stdio.h>\n"
      "#include <stdlib.h>\n"
      "#include \"yeah.h\"\n"
      (render (compile-rules rules))))

;(shew (compile-program prog))
(display (render-program prog))
