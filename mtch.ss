(define mtch-show-expansion #f)
;(define mtch-show-expansion #f)
;(set! mtch-show-expansion #f)

(define (shew . args) (map pretty-print args))

(define (mtch-literal? o)
  (or ;(is-quote? o)
      (string? o)
      (number? o)
      (null? o)
      (eq? #t o)
      (eq? #f o)))

(define (mtch-is-quote? o)
  (and (pair? o)
       (eq? (car o) 'quote)
       (pair? (cdr o))
       (null? (cddr o))))

(define mtch-vargen
  (let ((serial 0))
    (lambda ()
      (let ((s serial))
        (set! serial (+ serial 1))
        (string->symbol (string-append "_m" (number->string s)))))))

(define (mtch-clause target pat body)
  (let ((carvar (mtch-vargen))
        (cdrvar (mtch-vargen)))
    (cond
     ((mtch-is-quote? pat)
      `(if (equal? ',(cadr pat) ,target)
           ,body
           (fail)))     ((pair? pat)
      `(if (pair? ,target)
           (let ((,carvar (car ,target))
                 (,cdrvar (cdr ,target)))
             ,(mtch-clause carvar
                          (car pat)
                          (mtch-clause cdrvar
                                      (cdr pat)
                                      body)))
           (fail)))
     ((eq? '_ pat) body)
     ((symbol? pat)
      `(let ((,pat ,target)) ,body))
     ((mtch-literal? pat)
      `(if (equal? ',pat ,target)
           ,body
           (fail)))
     (#t (err 'mtch-clause target pat body)))))

(define (mtch-clauses target clauses all-clauses)
  (if (null? clauses)
      `(err 'match-failure target ',all-clauses)
      (let ((pat (car clauses))
            (body (cadr clauses))
            (rest (cddr clauses)))
        (let ((next (mtch-clauses target rest all-clauses)))
          `(let ((fail (lambda () ,next)))
             ,(mtch-clause target pat body))))))

(define (mtch-render target clauses)
  (let ((code
         `(let ((target ,target))
            ,(mtch-clauses 'target clauses clauses))))
    (if mtch-show-expansion
        (begin
          (pretty-print 'mtch)
          (pretty-print target)
          (pretty-print clauses)
          (pretty-print code))
        '())
    code))

(define-syntax mtch
  (syntax-rules ()
    ((_ target . clauses)
     (eval (mtch-render 'target 'clauses)))))

;(shew (mtch '(Foo 1) ('Foo a) `(ff ,a) ('Bar b c) `(,c ,b)))
;(shew (mtch '(Bar 2 3) ('Foo a) `(ff ,a) ('Bar b c) `(,c ,b)))
;(shew (expand '(mtch '(Bar 2 3) ('Foo a) `(ff ,a) ('Bar b c) `(,c ,b))))
;(shew (expand '(let ((ee '(1 .2))) (mtch ee (a . b) (cons b a) e e))))
