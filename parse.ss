(define grammar
  '((idchar (or ((literal #\a) (literal #\b) (literal #\c) (literal #\d) (literal #\e))))
    (identifier (or ((seq ((lhs idchar) (lhs identifier))) (lhs idchar))))
    (top (lhs identifier))))

;; returns new list, matched rule, and chars matched
;; _ () -> #f
;; (literal #\a) (#\a . rest) -> ((literal #\a) (cdr rest))
;; failure -> #f
(define (parse grammar grammar-exp chars)
  (if (null? chars)
      #f
      (mtch grammar-exp
            ('literal lit-exp)
            (if (eq? lit-exp (car chars))
                `((literal ,lit-exp) ,(cdr chars))
                #f)
            ('or alternatives) (parse-or grammar alternatives chars)
            ('seq exps) (parse-seq grammar exps chars)
            ('lhs name)
            (let ((parsed (parse grammar (car (lookup name grammar)) chars)))
              (mtch parsed
                    #f #f
                    (exp chars) `((node ,name ,exp) ,chars))))))

(define (parse-or grammar alternatives chars)
  (mtch alternatives
        (exp . exps)
        (let ((result (parse grammar exp chars)))
          (mtch result
                #f (parse-or grammar exps chars)
                _ result))
        () #f))

(define (parse-seq grammar exps chars)
  (mtch exps
        (exp . exps)
        (let ((result (parse grammar exp chars)))
          (mtch result
                (result-exp remaining)
                (let ((the-rest (parse-seq grammar exps remaining)))
                  (mtch the-rest
                        #f #f
                        (('seq results) remaining)
                        `((seq ,(cons result-exp results)) ,remaining)))
                #f #f))
        () `((seq ()) ,chars)))

;(tracefun parse)
(shew (parse grammar '(lhs top) '(#\a #\b #\c #\d)))

(ut
 (parse grammar '(literal #\a) '(#\a)) '((literal #\a) ())
 (parse grammar '(literal #\a) '(#\b)) #f
 (parse grammar '(literal #\a) '()) #f
 (parse grammar '(or ((literal #\a) (literal #\b))) '(#\a)) '((literal #\a) ())
 (parse grammar '(or ((literal #\a) (literal #\b))) '(#\b)) '((literal #\b) ())
 (parse grammar '(or ((literal #\a) (literal #\b))) '(#\c)) #f

 (parse-seq grammar '() '(#\a #\b)) '((seq ()) (#\a #\b))
 (parse-seq grammar '((literal #\a)) '(#\a #\b)) '((seq ((literal #\a))) (#\b))
 (parse-seq grammar '((literal #\a)) '(#\b #\a)) #f
 (parse-seq grammar '((literal #\a)) '(#\a #\b #\c)) '((seq ((literal #\a))) (#\b #\c))
 (parse-seq grammar '((literal #\c)) '(#\a #\b #\c)) #f
 (parse-seq grammar '((literal #\a) (literal #\b)) '(#\a #\b #\c)) '((seq ((literal #\a) (literal #\b))) (#\c))
 (parse-seq grammar '((literal #\a) (literal #\b)) '(#\a #\c #\b)) #f

 (parse grammar '(lhs idchar) '(#\a #\b)) '((node idchar (literal #\a)) (#\b))

 (parse grammar '(lhs identifier) '(#\a #\b #\c #\d))
 '((node
    identifier
    (seq ((node idchar (literal #\a)) (node identifier (seq
      ((node idchar (literal #\b)) (node identifier (seq ((node idchar (literal #\c)) (node identifier (node idchar (literal #\d))))))))))))
   ())
 (parse grammar '(lhs top) '(#\a #\b #\c #\d))
 '((node
    top
    (node
     identifier
     (seq ((node idchar (literal #\a)) (node identifier (seq
      ((node idchar (literal #\b)) (node identifier (seq ((node idchar (literal #\c)) (node identifier (node idchar (literal #\d)))))))))))))
   ())
 )

;;         ('lhs rule-name)
;;         (parse grammar (car (lookup rule-name grammar)) exp)
;;         ('literal literal-exp)
;;         (if (equal? literal-exp exp)

;;   (let* ((rule (lookup rule-name grammar)))
;;     (shew rule)
;;     (mtch (car rule)
;;           ('or alternatives)
;;           (parse-or alternatives exp))))

;; (define (parse-or alternatives exp)
;;   (match (parse 
