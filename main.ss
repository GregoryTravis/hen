;; (load "h.ss")
(load "lib.ss")

;; (run-file "src.ss")

(define (pat-ok? p)
  (if (pair? p)
      (and
       (proper-list? p)
       (if (eq? (car p) 'quote)
           (quote? p)
           (and
            (atom? (car p))
            (all pat-ok? (cdr p)))))
      #t))

(define (simplify-list-cdr d pat-p)
  (if (pair? d)
      (list 'pair (simplify (car d) pat-p) (simplify-list-cdr (cdr d) pat-p))
      (if (null? d)
          (list 'nil)
          (err d))))

(define (simplify-list p pat-p)
  (list 'pair (list 'literal (car p)) (simplify-list-cdr (cdr p) pat-p)))

(define (simplify p pat-p)
  (assert (pat-ok? p))
  (cond
   ((is-quote? p) p)
   ((pair? p) (simplify-list p pat-p))
   ((symbol? p) (if pat-p (list 'var p) (list 'literal p)))
   (#t (list 'literal p))))

(define (simplify-pat p) (simplify p #t))
(define (simplify-exp p) (simplify p #f))

(define (match p t)
  (let ((p-what (car p))
        (t-what (car t)))
    (cond
     ((and (eq? p-what 'nil)
           (eq? t-what 'nil))
      (just '()))
     ((and (eq? p-what 'pair)
           (eq? t-what 'pair))
      (maybe-append (match (cadr p) (cadr t))
                    (match (caddr p) (caddr t))))
     ((and (eq? p-what 'literal)
           (eq? t-what 'literal))
      (if
       (equal? (cadr p) (cadr t))
       (just '())
       fail))
     ((eq? p-what 'var)
      (just (list (list 'binding (cadr p) t))))
     (#t (err 'match p t)))))

(define (look-up-binding env varname)
  (if (null? env)
      fail
      (if (eq? varname (cadar env))
          (just (caddar env))
          (look-up-binding (cdr env) varname))))

(define (apply-env env body)
  (let ((b-what (car body)))
    (cond
     ((eq? b-what 'nil) body)
     ((eq? b-what 'pair)
      (list 'pair (apply-env env (cadr body)) (apply-env env (caddr body))))
     ((eq? b-what 'literal) body)
     ((eq? b-what 'var)
      (let ((binding (look-up-binding env (cadr body))))
        (if (fail? binding)
            (err 'unbound env body)
            (just-value binding))))
     (#t (err 'apply-env env body)))))

;(tracefun match apply-env look-up-binding simplify simplify-exp simplify-pat simplify-list simplify-list-cdr)

(define la
  '(
;    ((f b) (+ b b) (f 20))
;    ((f (ha b)) (* b b b) (f (ha 20)))
    ((f (bark a) (jok b t y)) (+ a y) (f (bark 20) (jok (vah 10) 20 (gurk 30 40))))
    ))

(map (lambda (p)
       (let* ((pat (car p))
              (body (cadr p))
              (t (caddr p))
              (spat (simplify-pat pat))
              (sbody (simplify-pat body))
              (st (simplify-exp t)))
         (shew pat body t spat sbody st)
         (let ((menv (match spat st)))
           (shew menv)
           (if (fail? menv)
               (err 'fail)
               (let ((rbody (apply-env (just-value menv) sbody)))
                 (shew rbody))))))
     la)
