(define (syntax-check p)
  (and
   (proper-tree? p)
   (no-aggregate-quotes p)))

(define (no-aggregate-quotes p)
  (treewalk (se-treewalker (list-treewalker
                            (lambda (e)
                              (assert (not (and
                                            (is-quote? e)
                                            (pair? (quote-quoted e)))) e))))
            p))
