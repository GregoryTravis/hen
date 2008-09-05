(load "check-syntax.ss")

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

(define (prepare-program p)
  (set! p (process-includes (cons '(include "overture.ss") p)))
  (assert (syntax-check p))
  p)

(define (load-files filenames)
  (let ((srcs (apply append (map (lambda (filename) (prepare-program (read-objects filename)))
                                 filenames))))
    (prepare-program srcs)))
