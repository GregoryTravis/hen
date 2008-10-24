(load "lib.ss")
(load "sb.ss")
(load "primitives.ss")
(load "assemble.ss")

(define (moch e pat)
  (mtch (list e pat)
        (b ('var a)) (list (list a b))
        (('pair ea eb) ('pair pa pb)) (append (moch ea pa) (moch eb pb))
        (('atom ea) ('atom pa)) (if (eq? ea pa) '() '(#f))
        (x y) (list #f)
        ))
(define (moch-failed bindings)
  (any? (map (feq? #f) bindings)))

(define (quote-first l)
  (cons `',(car l) (cdr l)))

(define (pairify l)
  (cond
   ((pair? l) `(pair ,(car l) ,(pairify (cdr l))))
   ((null? l) '(atom ()))
   (#t l)))

(define (syn a)
  (cond
   ((is-quote? a)
    (if (symbol? (quote-quoted a))
        `(atom ,(quote-quoted a))
        (err 'syn a)))
   ((or (number? a) (string? a)) `(atom ,a))
   ((proper-list? a)
    (pairify (map syn (quote-first a))))
   ((symbol? a) `(var ,a))
   ((null? a) '(atom ()))
   (#t (err 'syn a))))

(define (unsyn a)
  (mtch a
        ('pair a d) (cons (unsyn a) (unsyn d))
        ('atom a) a
        ('var a) (list 'unquote a)))

(define (unterse a)
  (cond
   ((integer? a) `(Integer (Primitive ,a)))
   ((string? a) `(String (Primitive ,a)))
   ((pair? a) (map unterse a))
   (#t a)))

(define (terse a)
  (mtch a
        ('Integer ('Primitive a)) a
        ('String ('Primitive a)) a
        (x . y) (map terse a)
        x x))

(define (apply-binding-to-body-var name bindings body)
  (let ((a (assoc name bindings)))
    (if (eq? a #f)
        (err 'bad-rhs-var name body bindings)
        (cadr a))))

(define (apply-bindings bindings body)
  (mtch body
        ('var v) (apply-binding-to-body-var v bindings body)
        ('pair a b) `(pair ,(apply-bindings bindings a)
                          ,(apply-bindings bindings b))
        ('atom a) body))

(define (rw target pat body)
  (let ((bindings (moch target pat)))
    (if (moch-failed bindings)
        #f
        (list (apply-bindings bindings body)))))

(define (rwrw target rws)
  (if (null? rws)
      #f
      (let* ((pat (caar rws))
             (body (cadar rws))
             (result (rw target pat body)))
        (if (eq? result #f)
            (rwrw target (cdr rws))
            result))))

(define (try-rwrw e rws)
  (let ((ee (rwrw e rws)))
    (if (eq? ee #f)
        e
        (car ee))))

(define (nmlz-list e rws)
  (mtch e
        ('pair a d) `(pair ,(nmlz a rws)
                           ,(nmlz-list d rws))
        x x))

(define (nmlz-step e rws)
  (mtch e
        ('atom a)
        e

        ('var v)
        (err 'normalized-to-var e)

        ('pair ('atom 'primitive-call) ('pair ('pair primfun args) (atom ())))
        (syn (do-primitive-call (unsyn `(pair ,primfun ,(nmlz-list args rws)))))

        ('pair a d)
        (try-rwrw (nmlz-list e rws) rws)))

(define (nmlz e rws)
  (let ((ee (nmlz-step e rws)))
    (if (equal? ee e)
        ee
        (nmlz ee rws))))

(define (prog->rules prog)
  (map (lambda (fun)
         (assert (fun-without-guard-syntax? fun))
         (list (syn (unterse (cadr fun))) (syn (unterse (caddr fun)))))
       prog))

(define (run prog)
  (let ((rules (prog->rules (grep fun-without-guard-syntax? prog)))
        (exps (grep (fnot fun-without-guard-syntax?) prog)))
    (map (lambda (e)
           (display "+ ")
           (lshew e)
           (display "\n")
           (let ((result (unsyn (nmlz (syn (unterse e)) rules))))
             (shew (terse result))
             result))
         exps)))
