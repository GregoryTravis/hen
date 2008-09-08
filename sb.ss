(define sb-debug #f)
(define sb-display-raw #f)

(define sb-barf-bletch 'blah-4-qq-4-qq-4)
(define sb-gak-nil 'zink-4-qq-4-qq-4)
(define sb-hwarf-dot 'goop-5-t-6-7)

(define (make-cs-displayer cf sf port)
  (lambda (s)
    (let* ((ss (sf s))
           (rendered (call-with-string-output-port
                      (lambda (port)
                        (pretty-print ss port))))
           (cc (cf rendered)))
      (if sb-debug (shew 'write 'orig s 'sf ss 'cf cc) '())
      (display cc port))))

(define (cs-filtered-read-all cf sf port)
  (let* ((c (apply ++ (read-all-lines port)))
         (cc (cf c))
         (scanneds (read-objects-port (open-string-input-port cc)))
         (sses (sf scanneds)))
    (if sb-debug (shew 'read 'orig c 'cf cc 'sf sses) '())
    sses))

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

(define (sb-unconsyize s)
  (assert (is-cons? s))
  (cond
   ((null? (caddr s)) (list (sb-out-cvt (cadr s))))
   ((is-cons? (caddr s)) (cons (sb-out-cvt (cadr s)) (sb-unconsyize (caddr s))))
   (#t (list (sb-out-cvt (cadr s)) sb-hwarf-dot (sb-out-cvt (caddr s))))))

(define (sb-out-cvt s)
  (cond
   ((is-quote? s) (list 'quote (sb-out-cvt (cadr s))))
   ((null? s) (list 'blah-4-qq-4-qq-4 'blah-4-qq-4-qq-4))
   ((is-cons? s) (cons sb-barf-bletch (snoc (sb-unconsyize s) sb-barf-bletch)))
   ((pair? s) (map sb-out-cvt s))
   ((atom? s) s)
   (#t s)))
   ;(#t (err 'sb-out-cvt s))))

(define (make-sb-displayer port)
  (make-cs-displayer
   (lambda (s)
     (set! s (regexp-replace* (++ "\\(" sb-barf-bletch " " sb-barf-bletch "\\)") s "[]"))
     (set! s (regexp-replace* (++ "\\(" sb-barf-bletch " ") s "["))
     (set! s (regexp-replace* (++ "\\(" sb-barf-bletch) s "["))
     (set! s (regexp-replace* (++ " " sb-barf-bletch "\\)") s "]"))
     (set! s (regexp-replace* (->string sb-hwarf-dot) s "."))
     (set! s (regexp-replace* (->string sb-gak-nil) s "[]"))
     s)
   (lambda (s) (sb-out-cvt (unpreprocess s)))
   port))

(define (sb-display-to-port s port)
  (if sb-display-raw
      (display s port)
      ((make-sb-displayer port) s)))

(define (sb-display s) (sb-display-to-port s (current-output-port)))

(define (lsb . args)
  (display (apply lssb args)))

(define (lssb . args)
  (string-collapse-spaces
   (string-one-line
    (call-with-output-string
     (lambda (port)
       (map (lambda (j) (sb-display-to-port j port)) args))))))

(define (sb s) (sb-display s))

(define (sb-consyize l)
  (cond
   ((null? l) '())
   ((pair? l) (list ''cons (sb-in-cvt (car l)) (sb-consyize (cdr l))))
   (#t (err))))

(define (sb-in-cvt s)
  (cond
   ((and (pair? s) (eq? sb-barf-bletch (car s)))
    (begin
      (assert (eq? sb-barf-bletch (caddr s)))
      (assert (= 3 (length s)))
      (map sb-in-cvt (sb-consyize (cadr s)))))
   ((pair? s) (map-improper sb-in-cvt s))
   ((atom? s) s)
   (#t (err 'sb-in-cvt))))

(define (make-sb-reader port)
  (make-cs-reader
   (lambda (s) (regexp-replace* "\\[" (regexp-replace* "\\]" s (++ ") " sb-barf-bletch ")")) (++ "(" sb-barf-bletch " (")))
   sb-in-cvt
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
