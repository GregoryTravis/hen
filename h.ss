(load "lib.ss")

(define (var? x)
  (symbol? x))

(define (rw? rw)
  (and (= 3 (length rw))
       (eq? (car rw) 'lambda)))

(define (compile-rewrite rw)
  (assert (rw? rw))
  (let ((arg (cadr rw))
        (body (caddr rw)))
    (cond
     ((var? arg)
      rw)
     ((pair? arg)
      (let* ((ooo (compile-rewrite `(lambda ,(cdr arg) ,body)))
             (aaa `(,ooo (cdr p)))
             (iii `(lambda ,(car arg) ,aaa)))
        `(lambda p
           (if (cons? p)
               (,iii (car p))
               (err)))))
     (#t (err compile-rewrite rw)))))

(define (reduce-app e)
  (let ((f (car e))
        (a (cadr e)))
    (assert rw? f)
    (let ((arg (cadr f))
          (body (caddr f)))
      (assert (var? arg))
      (cond
       ((symbol? body)
        (if (eq? body arg)
            a
            (err 'unbound-variable body 'in e) ))
       (#t (err 'reduce-app e))))))

(define (reduce e)
  (if (rw? e)
      e
      (reduce-app e)))

(define progs
  '(
;    ((lambda x x) (1 . 2))
    ((lambda (a . b) a) (1 . 2))
    ))

(tracefun reduce reduce-app)

(map
 (lambda (prog)
   (let* ((rw (car prog))
          (arg (cadr prog))
          (compiled-rw (compile-rewrite rw))
          (compiled-prog (list compiled-rw arg)))
     (shew rw compiled-rw)
     (shew (reduce compiled-prog))))
 progs)
