(require 2htdp/batch-io)
(require mzlib/string)

(define wonky-left-bracket-token 'L)
(define wonky-right-bracket-token 'R)
(define wonky-dot-token 'DOT)

(define wonky-left-bracket-replacement-string (++ "(" wonky-left-bracket-token " "))
(define wonky-right-bracket-replacement-string (++ " " wonky-right-bracket-token ")"))
(define wonky-dot-replacement-string (++ " " wonky-dot-token " "))

(define wonky-tokens (list wonky-left-bracket-token wonky-right-bracket-token wonky-dot-token))

(define (prelex-read filename) (prelex-string (read-file filename)))

(define (prelex-string file-contents-string) (listify-wonkiness (read-from-string-all (subst-prelex-tokens file-contents-string))))

(define (subst-prelex-tokens s)
  (let* ((s (regexp-replace* "\\[" s wonky-left-bracket-replacement-string))
         (s (regexp-replace* "\\]" s wonky-right-bracket-replacement-string))
         (s (regexp-replace* "\\." s wonky-dot-replacement-string)))
    s))

(define (replace-char-with-chars chars find replace)
  (cond
   ((and (pair? chars) (eq? (car chars) find)) (append replace (replace-char-with-chars (cdr chars) find replace)))
   ((pair? chars) (cons (car chars) (replace-char-with-chars (cdr chars) find replace)))
   ((null? chars) '())
   (#t (err 'bad-prelex-replace))))

(define (listify-wonkiness e)
  (cond
   ((and (pair? e) (eq? (car e) wonky-left-bracket-token)) (listify-wonkiness-cons-list (cdr e)))
   ((and (pair? e) (eq? (car e) wonky-dot-token) (not (null? (cdr e))) (null? (cddr e))) (listify-wonkiness (cadr e)))
   ((pair? e) (map listify-wonkiness e))
   ((member? e wonky-tokens) (err 'bad-prelex))
   (#t e)))

(define (listify-wonkiness-cons-list e)
  (cond
   ((and (pair? e) (eq? (car e) wonky-right-bracket-token) (null? (cdr e))) 'Nil)
   ((pair? e) `(Cons ,(listify-wonkiness (car e)) ,(listify-wonkiness-cons-list (cdr e))))
   ((null? e) (err 'bad-prelex))
   (#t (listify-wonkiness e))))

;(tracefun subst-prelex-tokens listify-wonkiness listify-wonkiness-cons-list)

(define (un-prelex-write filename e) (write-file filename (un-prelex-to-string e)))

(define (sexp-to-string e) (with-output-to-string (lambda () (write e))))

(define (un-prelex-to-string e) (unsubst-prelex-tokens (apply ++ (map sexp-to-string (unlistify-wonkiness e)))))

(define (unlistify-wonkiness e)
  (mtch e
        ('Cons a b) (cons wonky-left-bracket-token (append (map unlistify-wonkiness (unlistify-wonkiness-listified-list e)) (list wonky-right-bracket-token)))
        'Nil (list wonky-right-bracket-token)
        (a . d) (map unlistify-wonkiness e)
        _ e))

(define (unlistify-wonkiness-listified-list e)
  (mtch e
        ('Cons a d) (cons a (unlistify-wonkiness-listified-list d))
        'Nil '()))

(define (unsubst-prelex-tokens s)
  (let* ((s (regexp-replace* (regexp-quote wonky-left-bracket-replacement-string) s "["))
         (s (regexp-replace* (regexp-quote wonky-right-bracket-replacement-string) s "]"))
         (s (regexp-replace* (regexp-quote wonky-dot-replacement-string) s ".")))
    s))
