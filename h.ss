(load "lib.ss")

(define (top-rw pat t body)
  (rw pat t body body))

(define (subst var x body-template body)
  (cond
   ((eq? 'fail body) 'fail)
   ((literal? body) body)
   ((and (symbol? body-template)
         (eq? var body-template))
    x)
   ((pair? body-template)
    (cons (subst var x (car body-template) (car body))
          (subst var x (cdr body-template) (cdr body))))
   ((symbol? body) body)
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
            result))))

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

;; (define (quote-firsts e)
;;   (cond
;;    ((literal? e) e)
;;    ((fun? e)
;;     `(fun ,(quote-firsts (cadr e))
;;           ,(quote-firsts (caddr e))))
;;    ((and (app? e) (symbol? (car e)))
;;     `((quote ,(car e)) ,@(cdr e)))
;;    (#t e)))

;; (define (preprocess src)
;;   (map quote-firsts src))

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

(define (quote-firsts e)
  (cond
   ((literal? e) e)
   ((symbol? e) e)
   ((and (app? e) (symbol? (car e)))
    `(',(car e) ,@(map quote-firsts (cdr e))))
   ((app? e) (map quote-firsts e))
   (#t (err 'quote-firsts e))))

(define (preprocess src)
  (map quote-non-variables src))

(define (gather-rws src) (grep fun? src))
(define (gather-exps src) (grep (fnot fun?) src))

(define (run-src src)
  (let ((src (preprocess src)))
    (let ((rws (gather-rws src))
          (exps (gather-exps src)))
      (map (lambda (e) (top-evl e rws)) exps))))

(define (top-evl e rws)
  (display "+ ")
  (shew e)
  (let ((r (normalize e rws)))
    (shew r)
    (display "\n")
    r))

;(tracefun try-rws)
;(tracefun subst rw top-rw try-rws-top)
;(tracefun normalize)
;(tracefun literal? app?)
;(tracefun quote-firsts gather-binders quote-symbols quote-symbols-except-these)
;(tracefun quote-non-variables)

(define (go)
  (run-src (read-objects "src.ss")))

(go)
