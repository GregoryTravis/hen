(define (make-cs-displayer cf sf port)
  (lambda (s)
    (let* ((ss (sf s))
           (rendered (call-with-string-output-port
                      (lambda (port)
                        (display ss port))))
           (cc (cf rendered)))
      (shew 'orig s 'sf ss 'cf cc)
      (display cc))))

(define (cs-filtered-read-all cf sf port)
  (let ((s (apply ++ (read-all-lines port))))
    (let* ((ro (read-objects-port (open-string-input-port (cf s))))
           (barf (sf ro)))
                                        ;(shew 'ro ro 'barf barf)
      barf)))

(define (make-cs-reader cf sf port)
  (let ((stack '()))
    (lambda ()
      (if (null? stack)
          (set! stack (cs-filtered-read-all cf sf port))
          '())
      (if (null? stack)
          eof
          (let ((top (car stack)))
            (set! stack (cdr stack))
            top)))))

(define (sq-unconsyize s)
  (assert (is-quoted-cons? s))
  (cond
   ((null? (caddr s)) (list (sq-out-cvt (cadr s))))
   ((is-quoted-cons? (caddr s)) (cons (sq-out-cvt (cadr s)) (sq-unconsyize (caddr s))))
   (#t (list (sq-out-cvt (cadr s)) sb-hwarf-dot (sq-out-cvt (caddr s))))))

(define (sq-out-cvt s)
  (cond
   ((is-quoted-cons? s) (cons sb-barf-bletch (snoc (sq-unconsyize s) sb-barf-bletch)))
   ((pair? s) (map-improper sq-out-cvt s))
   ((atom? s) s)
   (#t (err 'sq-out-cvt))))

(define (make-sb-displayer port)
  (make-cs-displayer
   (lambda (s)
     (set! s (regexp-replace* (++ "\\(" sb-barf-bletch " ") s "["))
     (set! s (regexp-replace* (++ " " sb-barf-bletch "\\)") s "]"))
     (set! s (regexp-replace* (->string sb-hwarf-dot) s "."))
     (set! s (regexp-replace* (->string sb-gak-nil) s "[]"))
     s)
   sq-out-cvt
   port))

(define (sb-display s)
  ((make-sb-displayer (current-output-port)) s))

(define (sq-consyize l)
  (if (pair? l)
      (list ''cons (sq-in-cvt (car l)) (sq-consyize (cdr l)))
      (sq-in-cvt l)))

(define (sq-in-cvt s)
  (cond
   ((and (pair? s) (eq? sb-barf-bletch (car s)))
    (begin
      (assert (eq? sb-barf-bletch (caddr s)))
      (assert (= 3 (length s)))
      (sq-consyize (cadr s))))
   ((pair? s) (map-improper sq-in-cvt s))
   ((atom? s) s)
   (#t (err 'sq-in-cvt))))

(define (make-sb-reader port)
  (make-cs-reader
   (lambda (s) (regexp-replace* "\\[" (regexp-replace* "\\]" s (++ ") " sb-barf-bletch ")")) (++ "(" sb-barf-bletch " (")))
   sq-in-cvt
   port))

(define (sb-read sb-reader)
  (sb-reader))

(define (sb-read-objects sb-reader)
  (let ((v (sb-read sb-reader)))
    (if (eof-object? v)
        '()
        (cons v (sb-read-objects sb-reader)))))

(define (sb-read-file filename)
  (call-with-input-file filename
    (lambda (port) (sb-read-objects (make-sb-reader port)))))
