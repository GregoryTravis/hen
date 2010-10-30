(require 2htdp/batch-io)
(require mzlib/string)

(define wonky-left-bracket-token 'L)
(define wonky-right-bracket-token 'R)
(define wonky-dot-token 'DOT)

(define wonky-left-bracket-replacement-string (++ "(" wonky-left-bracket-token))
(define wonky-right-bracket-replacement-string (++ wonky-right-bracket-token ")"))
(define wonky-dot-replacement-string (->string wonky-dot-token))

(define wonky-tokens (list wonky-left-bracket-token wonky-right-bracket-token wonky-dot-token))

(define (prelex-read filename) (prelex-string (read-file filename)))

(define (prelex-string file-contents-string) (listify-wonkiness (read-from-string-all (subst-prelex-tokens file-contents-string))))

(define (subst-prelex-tokens s)
  (let* ((s (regexp-replace* "\\[" s (++ wonky-left-bracket-replacement-string " ")))
         (s (regexp-replace* "\\]" s (++ " " wonky-right-bracket-replacement-string)))
         (s (regexp-replace* "\\." s (++ " " wonky-dot-replacement-string " "))))
    s))

(define (replace-char-with-chars chars find replace)
  (cond
   ((and (pair? chars) (eq? (car chars) find)) (append replace (replace-char-with-chars (cdr chars) find replace)))
   ((pair? chars) (cons (car chars) (replace-char-with-chars (cdr chars) find replace)))
   ((null? chars) '())
   (#t (err 'bad-prelex-replace))))

(define (listify-wonkiness e)
  (listify-wonkiness-dots (listify-wonkiness-square-brackets e)))

(define (listify-wonkiness-square-brackets e)
  (cond
   ((and (pair? e) (eq? (car e) wonky-left-bracket-token)) (listify-wonkiness-cons-list (cdr e)))
   ((pair? e) (map listify-wonkiness-square-brackets e))
   ((or (eq? e wonky-left-bracket-token) (eq? e wonky-right-bracket-token)) (err 'bad-prelex))
   (#t e)))

(define (listify-wonkiness-cons-list e)
  (cond
   ((and (pair? e) (eq? (car e) wonky-right-bracket-token) (null? (cdr e))) 'Nil)
   ((pair? e) `(Cons ,(listify-wonkiness-square-brackets (car e)) ,(listify-wonkiness-cons-list (cdr e))))
   ((null? e) (err 'bad-prelex))
   (#t (listify-wonkiness-square-brackets e))))

(define (listify-wonkiness-dots e)
  (cond
   ((mtch e ('Cons a ('Cons b 'Nil)) (eq? a wonky-dot-token) _ #f) (listify-wonkiness-dots (cadr (caddr e))))
   ((mtch e ('Cons a ('Cons b _)) (eq? a wonky-dot-token) _ #f) (err 'bad-prelex))
   ((and (pair? e) (eq? (car e) wonky-dot-token) (pair? (cdr e)) (not (pair? (cddr e)))) (listify-wonkiness-dots (cadr e)))
   ((pair? e) (cons (listify-wonkiness-dots (car e)) (listify-wonkiness-dots (cdr e))))
   ((eq? e wonky-dot-token) (err 'bad-prelex))
   (#t e)))

;(tracefun subst-prelex-tokens listify-wonkiness listify-wonkiness-cons-list listify-wonkiness-square-brackets listify-wonkiness-dots)

(define (un-prelex-write filename e) (write-file filename (un-prelex-to-string e)))

(define (sexp-to-string e) (with-output-to-string (lambda () (pretty-print e))))

(define (un-prelex-to-string e) (unsubst-prelex-tokens (sexp-to-string (unlistify-wonkiness e))))

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
  (let* ((s (regexp-replace* (++ (regexp-quote wonky-left-bracket-replacement-string) " *") s "["))
         (s (regexp-replace* (++ " *" (regexp-quote wonky-right-bracket-replacement-string)) s "]"))
         (s (regexp-replace* (regexp-quote wonky-dot-replacement-string) s ".")))
    s))
