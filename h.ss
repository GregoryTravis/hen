(load "lib.ss")
(load "sb.ss")
(load "primitives.ss")

(define show-reductions #f)
(define show-normalizations #f)
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

(define (top-rw rule t)
  (let ((pat (cadr rule))
        (body (caddr rule)))
    (rw pat t body body)))

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
   ((and (pair? pat))
    (if (pair? t)
        (rw (cdr pat) (cdr t) body-template (rw (car pat) (car t) body-template body))
        'fail))
   ((and (symbol? pat))
    (subst pat t body-template body))
   (#t (err 'rw pat t body-template body))))

(define (try-rws e rws)
  (if (null? rws)
      'fail
      (let* ((rule (car rws))
             (result (top-rw rule e)))
        (if (eq? 'fail result)
            (try-rws e (cdr rws))
            result))))

(define (normalize-children e rws)
  (map (lambda (e) (normalize e rws)) e))

(define (primitive-call? e)
  (is-this-labeled-doublet? ''primitive-call e))

(define (do-conditional e rws)
  (let ((pred (cadr e))
        (then (caddr e))
        (else (cadddr e)))
    (let ((b (normalize pred rws)))
      (cond
       ((equal? ''true b) (normalize then rws))
       ((equal? ''false b) (normalize else rws))
       (#t (err 'conditional 'pred pred 'gives b))))))

(define (normalize e rws)
  (cond
   ((primitive-call? e)
    (do-primitive-call (cadr e)))
   ((conditional? e)
    (do-conditional e rws))
   (#t (let* ((ee (if (app? e)
                      (normalize-children e rws)
                      e))
              (r (try-rws ee rws)))
         (cond
          ((or (eq? 'fail r) (equal? r ee)) ee)
          (#t (normalize r rws)))))))

(define (quote-symbols e)
  (quote-symbols-except-these e '()))

(define (quote-symbols-except-these e except)
  (cond
   ((literal? e) e)
   ((and (symbol? e) (member? e except)) e)
   ((symbol? e) `',e)
   ((app? e) (map (lambda (e) (quote-symbols-except-these e except))
                  e))
   (#t (err 'quote-symbols-except-these e))))

(define (gather-binders pat)
  (cond
   ((symbol? pat) (list pat))
   ((literal? pat) '())
   ((pair? pat) (append (gather-binders (car pat))
                       (gather-binders (cdr pat))))
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
   ((and (pair? e) (symbol? (car e)))
    `(',(car e) ,@(map-improper quote-firsts (cdr e))))
   ((pair? e) (map-improper quote-firsts e))
   (#t (err 'quote-firsts e))))

(define (list->quoted-consy l)
  (foldr (lambda (a d) `('cons ,a ,d)) '() l))

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
  (set! src (primitivize src))
  (set! src (map quote-non-variables src))
  src)

(define (unpreprocess src)
  (set! src (unquote-non-variables src))
  (set! src (unprimitivize src))
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
  (normalize e rws))

(define (top-rw-dumper rule e result)
  (if (not (eq? 'fail result))
      (begin
        ;(display "<== ")
        (lsb e)
        (display "\n==> ")
        (lsb result)
        (display "\n                       ** ")
        (lsb rule)
        (display "\n"))
  '()))

(define (normalize-dumper e rws result)
  (begin
    (lsb e)
    (display "\n==> ")
    (lsb result)
    (display "\n")))

(define (top-evl-dumper e rws result)
  (display "+ ")
  (sb e)
  (sb result)
  (show-counts)
  (display "\n"))

(define (top-rw-ticker rule t result)
  (if (not (eq? 'fail result))
      (tick-rw)
      '())
  result)

;(if show-reductions (hook-with (args-and-result-hook top-rw-dumper) top-rw) '())
(if show-normalizations (hook-with (args-and-result-hook normalize-dumper) normalize) '())
(if (not show-normalizations) (hook-with (args-and-result-hook top-evl-dumper) top-evl) '())
(if show-counts (hook-with (args-and-result-hook top-rw-ticker) top-rw) '())

(define already-including '())

(define (process-includes forms)
  (cond
   ((null? forms) '())
   ((and (pair? forms)
         (pair? (car forms))
         (eq? 'include (caar forms)))
    (let ((includee (cadar forms)))
      (if (not (member includee already-including))
          (begin
            ;(shew 'including includee)
            (set! already-including (cons includee already-including))
            (append (process-includes (read-objects includee))
                    (process-includes (cdr forms))))
          (begin
            ;(shew 'not-including includee)
            (process-includes (cdr forms))))))
   ((pair? forms)
    (cons (car forms) (process-includes (cdr forms))))
   (#t (err))))

(define (run-file filename)
  (run-src (process-includes (cons '(include "overture.ss")
                                   (sb-read-file filename)))))

(define (go)
  (run-file "src.ss"))
