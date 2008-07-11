(load "lib.ss")

(define show-counts-p #f)
(define counts-rws 0)

(define (show-counts)
  (if show-counts-p
      (begin
        (display "[")
        (display counts-rws)
        (display " reductions]\n"))
      '()))

(define (reset-counts)
  (set! counts-rws 0))

(define (tick-rw)
  (set! counts-rws (+ counts-rws 1)))

(define (top-rw pat t body)
  (rw pat t body body))

(define (subst var x body-template body)
  (cond
   ((eq? 'fail body) 'fail)
   ((literal? body-template) body)
   ((and (symbol? body-template)
         (eq? var body-template))
    x)
   ((pair? body-template)
    (cons (subst var x (car body-template) (car body))
          (subst var x (cdr body-template) (cdr body))))
   ((symbol? body-template) body)
   (#t (err 'subst var x body-template body))))

(define (rw pat t body-template body)
  (cond
   ((and (literal? pat))
    (if (equal? pat t)
        body
        'fail))
   ((and (app? pat))
    (if (app? t)
        (rw (cdr pat) (cdr t) body-template (rw (car pat) (car t) body-template body))
        'fail))
   ((and (symbol? pat))
    (subst pat t body-template body))
   (#t (err 'rw pat t body-template body))))

(define (try-rws e rws)
  (if (null? rws)
      'fail
      (let* ((rule (car rws))
             (pat (cadr rule))
             (body (caddr rule))
             (result (top-rw pat e body)))
        (if (eq? 'fail result)
            (try-rws e (cdr rws))
            (begin
              (tick-rw)
              result)))))

(define (try-rws-top e rws)
  (try-rws e rws))

(define (normalize-children e rws)
  (map (lambda (e) (normalize e rws)) e))

(define (normalize e rws)
  (let* ((ee (if (app? e)
                 (normalize-children e rws)
                 e))
         (r (try-rws ee rws)))
    (cond
     ((eq? 'fail r) ee)
     ((equal? r ee) ee)
     (#t (normalize r rws)))))

(define (quote-symbols e)
  (quote-symbols-except-these e '()))

(define (quote-symbols-except-these e except)
  (cond
   ((literal? e) e)
   ((and (symbol? e) (member e except)) e)
   ((symbol? e) `',e)
   ((app? e) (map (lambda (e) (quote-symbols-except-these e except))
                  e))
   (#t (err 'quote-symbols-except-these e))))

(define (gather-binders pat)
  (cond
   ((symbol? pat) (list pat))
   ((literal? pat) '())
   ((app? pat) (map-append gather-binders pat))
   (#t (err 'gather-binders))))

(define (quote-non-variables e)
  (if (fun? e)
      (let* ((pat (cadr e))
             (body (caddr e))
             (qpat (quote-firsts pat)))
        `(fun ,qpat
              ,(quote-symbols-except-these body (gather-binders qpat))))
      (quote-symbols e)))

(define (unquote-non-variables e)
  (unquote-firsts e))

(define (unquote-firsts e)
  (cond
   ((literal? e) e)
   ((and (list? e) (is-quote? (car e)))
    `(,(quote-quoted (car e)) ,@(map unquote-firsts (cdr e))))
   ((list? e) (map unquote-firsts e))
   ((atom? e) e)
   (#t (err 'unquote-firsts e))))

(define (quote-firsts e)
  (cond
   ((literal? e) e)
   ((symbol? e) e)
   ((and (app? e) (symbol? (car e)))
    `(',(car e) ,@(map quote-firsts (cdr e))))
   ((app? e) (map quote-firsts e))
   (#t (err 'quote-firsts e))))

(define (quoted-lists-to-consy-lists e)
  (cond
   ((and (is-quote? e) (list? (cadr e)))
    (foldr (lambda (a d) `(cons ,a ,d)) '() (cadr e)))
   ((app? e) (map quoted-lists-to-consy-lists e))
   ((atom? e) e)
   (#t (err 'quoted-lists-to-consy-lists e))))

(define (consy-list-to-quoted-lists e)
  `(quote ,(consy-list-to-quoted-lists-1 e)))

(define (consy-list-to-quoted-lists-1 e)
  (assert (or (null? e) (is-cons? e)))
  (if (null? e)
      '()
      (cons (consy-lists-to-quoted-lists (cadr e))
            (consy-list-to-quoted-lists-1 (caddr e)))))

(define (consy-lists-to-quoted-lists e)
  (cond
   ((is-cons? e) (consy-list-to-quoted-lists e))
   ((pair? e) (map consy-lists-to-quoted-lists e))
   ((atom? e) e)
   (#t (err 'consy-lists-to-quoted-lists))))

(define (primitivize e)
  (atom-traverse
   (lambda (e)
     (cond
      ((integer? e) `(integer (primitive ,e)))
      ((string? e) `(string (primitive ,e)))
      (#t e)))
   e))

(define (unprimitivize e)
  (cond
   ((is-quote? e) `(quote ,(unprimitivize (cadr e))))
   ((is-some-primitive? e) (cadadr e))
   ((list? e) (map unprimitivize e))
   (#t e)))

(define (preprocess src)
  (set! src (quoted-lists-to-consy-lists src))
  (set! src (primitivize src))
  (set! src (map quote-non-variables src))
  src)

(define (unpreprocess src)
  (set! src (unquote-non-variables src))
  (set! src (unprimitivize src))
  (set! src (consy-lists-to-quoted-lists src))
  src)

(define (gather-rws src) (grep fun? src))
(define (gather-exps src) (grep (fnot fun?) src))

(define (run-src src)
  (let* ((src (preprocess src))
         (rws (gather-rws src))
         (exps (gather-exps src)))
    (map (lambda (e) (top-evl e rws)) exps)))

(define (top-evl e rws)
  (reset-counts)
  (let ((ue (unpreprocess e)))
    (display "+ ")
    (shew ue)
    (let ((r (normalize e rws)))
      (shew (unpreprocess r))
      (show-counts)
      (display "\n")
      r)))

;(tracefun try-rws)
;(tracefun subst rw top-rw try-rws-top)
;(tracefun normalize normalize-children)
;(tracefun literal? app?)
;(tracefun quote-firsts unquote-firsts gather-binders quote-symbols quote-symbols-except-these quote-non-variables unquote-non-variables)
;(tracefun preprocess unpreprocess primitivize unprimitivize)
;(tracefun consy-lists-to-quoted-lists consy-list-to-quoted-lists consy-list-to-quoted-lists-1 quoted-lists-to-consy-lists)
;(tracefun is-some-primitive? is-this-labeled-doublet? is-this-primitive? primitive?)

(define (run-file filename)
  (run-src (read-objects filename)))

(define (go)
  (run-file "src.ss"))
