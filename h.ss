(load "lib.ss")
(load "primitives.ss")

(define show-counts-p #f)
(define counts-rws 0)

(define sb-barf-bletch 'blah-4-qq-4-qq-4)

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

(define (primitive-call? e)
  (is-this-labeled-doublet? ''primitive-call e))

(define (normalize e rws)
  (cond
   ((primitive-call? e)
    (do-primitive-call (cadr e)))
   ((conditional? e)
    (let ((pred (cadr e))
          (then (caddr e))
          (else (cadddr e)))
      (let ((b (normalize pred rws)))
        (cond
         ((equal? ''true b) (normalize then rws))
         ((equal? ''false b) (normalize else rws))
         (#t (err 'conditional 'pred pred 'gives b))))))
   (#t (let* ((ee (if (app? e)
                      (normalize-children e rws)
                      e))
              (r (try-rws ee rws)))
         (cond
          ((eq? 'fail r) ee)
          ((equal? r ee) ee)
          (#t (normalize r rws)))))))

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

(define (undo-square-brackety e)
  (cond
   ((and (pair? e) (eq? sb-barf-bletch (car e)))
    (begin
      (assert (eq? sb-barf-bletch (last e)))
      (map undo-square-brackety
           (foldr (lambda (a d) `(cons ,a ,d))
                  '()
                  (rdc (cdr e))))))
   ((pair? e) (map undo-square-brackety e))
   ((atom? e) e)
   (#t (err 'undo-square-brackety e))))

(define (consy-list-to-square-bracket-list e)
  `(,sb-barf-bletch ,@(consy-list-to-square-bracket-list-1 e) ,sb-barf-bletch))

(define (consy-list-to-square-bracket-list-1 e)
  (assert (or (null? e) (is-cons? e)))
  (if (null? e)
      '()
      (cons (do-square-brackety (cadr e))
            (consy-list-to-square-bracket-list-1 (caddr e)))))

(define (do-square-brackety e)
  (cond
   ((is-cons? e) (consy-list-to-square-bracket-list e))
   ((pair? e) (map do-square-brackety e))
   ((atom? e) e)
   (#t (err 'do-square-brackety))))


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
  ;(set! src (undo-square-brackety src))
  (set! src (primitivize src))
  (set! src (map quote-non-variables src))
  src)

(define (unpreprocess src)
  (set! src (unquote-non-variables src))
  (set! src (unprimitivize src))
  (set! src (do-square-brackety src))
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
                                   (pea-read-src filename)))))

(define (read-file-string filename)
  (call-with-input-file filename (lambda (p) (get-string-all p))))

(define (pea-parsing-port port)
  (open-string-input-port (preprocess-square-brackety (get-string-all (transcoded-port port (native-transcoder))))))

(define (pea-read-src-port port)
  (undo-square-brackety (read-objects-port (pea-parsing-port port))))

(define (pea-read-src filename)
  (call-with-input-file filename pea-read-src-port))

(define (preprocess-square-brackety s)
  (set! s (pregexp-replace* "\\[" s "(blah-4-qq-4-qq-4 "))
  (set! s (pregexp-replace* "\\]" s " blah-4-qq-4-qq-4)"))
  s)

;(tracefun try-rws)
;(tracefun subst rw top-rw try-rws-top)
;(tracefun normalize normalize-children)
;(tracefun literal? app?)
;(tracefun quote-firsts unquote-firsts gather-binders quote-symbols quote-symbols-except-these quote-non-variables unquote-non-variables)
;(tracefun preprocess unpreprocess primitivize unprimitivize)
;(tracefun consy-lists-to-quoted-lists consy-list-to-quoted-lists consy-list-to-quoted-lists-1 quoted-lists-to-consy-lists)
;(tracefun is-some-primitive? is-this-labeled-doublet? is-this-primitive? primitive?)
;(tracefun do-primitive-call)
;(tracefun extract-primitive-maybe)
;(tracefun conditional?)
;(tracefun process-includes)

(define (go)
  (run-file "src.ss"))
