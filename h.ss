;; (load "h.ss")
(load "lib.ss")

(define prog
  '((Rule ((Lit foo) (Var a) (Var b)) ((Lit Jerk) (Var b) (Var a)))
    (Rule ((Lit foo) (Var a)) ((Lit Jick) (Var a) (Var a)))))

(define (compile-pseudofunction rules)
  ;(join-things "\n\n" (map compile-rule rules)))
  `(alternatives ,(map compile-rule rules)))

(define (compile-rule rule)
  (mtch rule
        (Rule pat body) (compile-pat pat body)))

(define (compile-pat pat body)
  (compile-pat-1 'r pat `(build ,body)))

(define (carsym o) (->symbol (++ o 'a)))
(define (cdrsym o) (->symbol (++ o 'd)))

(define (compile-pat-1 var pat body)
  (mtch pat
        ('Lit lit) `(if (eq? ,var ,lit)
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
        ('alternatives alts)
        (join-things "\n" (map render alts))

        ('if b t)
        (++ "if (" (render b) ") {\n" (render t) "\n}")

        ('pair? var)
        (++ "ispair(" var ")")

        ('let* assignments body)
        (++ (join-things "\n" (map render-assignment assignments)) "\n" (render body))

        ('dummy) ";"

        ('assign var exp body) (++ (render-assignment `(,var ,exp)) "\n" (render body))

        ('car e) (++ "car(" (render e) ")")
        ('cdr e) (++ "cdr(" (render e) ")")
        ('null? e) (++ "isnil(" (render e) ")")

        ('build b) (render-build b)

        otherwise p))

(define (render-build b)
  (mtch b
        ('Lit sym) (++ "mksymbol(\"" sym "\")")
        ('Var var) var
        (a . d) (++ "mkpair(" (render-build a) ", " (render-build d) ")")
        () "nil()"))

;(shew (compile-pseudofunction prog))
(display (render (compile-pseudofunction prog)))
