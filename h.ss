;; (load "h.ss")
(load "lib.ss")

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
     (#t (err 'match p-what t-what p t)))))

(define (look-up-binding env varname)
  (if (null? env)
      fail
      (if (eq? varname (cadar env))
          (just (caddar env))
          (look-up-binding (cdr env) varname))))

(define (apply-match-env env body)
  (let ((b-what (car body)))
    (cond
     ((eq? b-what 'nil) body)
     ((eq? b-what 'pair)
      (list 'pair (apply-match-env env (cadr body)) (apply-match-env env (caddr body))))
     ((eq? b-what 'literal) body)
     ((eq? b-what 'var)
      (let ((binding (look-up-binding env (cadr body))))
        (if (fail? binding)
            (err 'unbound env body)
            (just-value binding))))
     (#t (err 'apply-match-env env body)))))

(define (hshew . args)
  (apply shew (map unsimplify args)))

(define (unsimplify o)
  (cond
   ((eq? (car o) 'pair)
    (cons (unsimplify (cadr o))
          (unsimplify (caddr o))))
   ((eq? (car o) 'literal)
    (cadr o))
   ((eq? (car o) 'nil) '())
   ((eq? (car o) 'var) (cadr o))
   (#t (err 'unsimplify o))))

(define (fun? p)
  (and (pair? p)
       (eq? (car p) 'fun)
       (and (= 3 (length p)))))

(define (run-file f)
  (run-file-src (read-objects f)))

(define rewrite-rules '())

(define (define-rule o)
  (assert (fun? o))
  (let* ((pat (cadr o))
         (body (caddr o))
         (simplified-rule (list (simplify-pat pat) (simplify-pat body))))
    (set! rewrite-rules (snoc rewrite-rules simplified-rule))
    (display "* ")
    (shew o)))

(define (try-a-rule simplified-e)
  (find-first-maybe
   (lambda (rewrite)
     (let ((simplified-pat (car rewrite))
           (simplified-body (cadr rewrite)))
       (let ((matched-maybe (match simplified-pat simplified-e)))
         (if (fail? matched-maybe)
             fail
             (just (apply-match-env (just-value matched-maybe) simplified-body))))))
   rewrite-rules))

(define (exec-exp e)
  (let ((e (simplify-exp e)))
    (display "+ ")
    (hshew e)
    (display "    =>\n")
    (let ((rewrite-maybe (try-a-rule e)))
      (if (fail? rewrite-maybe)
          (display "Cain't.\n")
          (begin
            (display "  ")
            (hshew (just-value rewrite-maybe)))))))

(define (exec-top-level-form o)
  (if (fun? o)
      (define-rule o)
      (exec-exp o)))

(define (run-file-src forms)
  (map exec-top-level-form forms))

;(tracefun match)
