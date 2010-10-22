(require 2htdp/batch-io)
(require mzlib/string)

(define wonky-left-bracket-token 'L)
(define wonky-right-bracket-token 'R)
(define wonky-dot-token 'DOT)

(define wonky-left-bracket-replacement-string (string->list (++ "(" wonky-left-bracket-token " ")))
(define wonky-right-bracket-replacement-string (string->list (++ " " wonky-right-bracket-token ")")))
(define wonky-dot-replacement-string (string->list (++ " " wonky-dot-token " ")))

(define wonky-tokens (list wonky-left-bracket-token wonky-right-bracket-token wonky-dot-token))

(define (prelex-read filename) (prelex-string (read-file filename)))

(define (prelex-string file-contents-string) (listify-wonkiness (read-from-string-all (list->string (subst-prelex-tokens (string->list file-contents-string))))))

(define (subst-prelex-tokens chars)
  (let* ((chars (replace-char-with-chars chars #\[ wonky-left-bracket-replacement-string))
         (chars (replace-char-with-chars chars #\] wonky-right-bracket-replacement-string))
         (chars (replace-char-with-chars chars #\. wonky-dot-replacement-string)))
    chars))

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
