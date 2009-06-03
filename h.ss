(load "lib.ss")
(load "util.ss")
(load "primitives.ss")

(define (constant? a) (or (number? a) (null? a) (ctor? a)))
(define (quote-constant a) (mtch a '() ''() x x))

(define (fun? e) (mtch e ('fun a b) #t _ #f))
(define (fun-name fun) (mtch fun ('fun (name . args) body) name))
(define (fun->clause fun) (mtch fun ('fun (name . args) body) `(Clause ,args ,body)))

;(tracefun simplify-patterns patterns->conditionals ->scheme)
;(tracefun def->scheme gort)

(define (fun->question-trees fun)
  (mtch fun
        ('Fun name clauses)
        `(Fun ,name ,(merge-question-paths (map clause->question-path (map pathize-clause clauses))))))

(define (merge-question-paths paths)
  (mtch paths
        (()) paths
        ((('Body body))) `(Body ,body)
        _
        (let ((grouped (group-by car paths)))
                                        ;        (shew 'grouped grouped)
          (set! grouped (map (lambda (sub) (list (car sub) (map cdr (cadr sub)))) grouped))
                                        ;        (shew 'grouped grouped)
          (set! grouped (map (lambda (sub) (list (car sub) (merge-question-paths (cadr sub)))) grouped))
                                        ;        (shew 'grouped grouped)
          grouped)))

;;         (let ((grouped (lensmap cdr-lens ($ map cdr _) grouped)))
;;           (shew 'grouped grouped)
;;           (lensmap cdr-lens merge-question-paths grouped)))))

(tracefun merge-question-paths)

(define (clause->question-path . args)
  (mtch
   args
   (('Clause args body)) (append (clause->question-path args "lv") `((Body ,body)))

   (arg var-path)
   (cond
    ((pair? arg)
     (cons `(Question (pair? ,(->symbol var-path)))
           (append (clause->question-path (car arg) (++ var-path "a"))
                   (clause->question-path (cdr arg) (++ var-path "d")))))
    ((constant? arg)
     `((Question (eq? ,(->symbol var-path) ',arg))))
    ((symbol? arg)
     '())
;     `((Question (let ,arg ,(->symbol var-path)))))
    (#t (err 'clause->question-path args)))))

(define (pathize-clause stuff)
  (mtch stuff
        ('Clause args body)
        (let ((renaming (pathize-pattern-variables args)))
          `(Clause ,(subst-variables args renaming)
                   ,(subst-variables body renaming)))))

(define (pathize-pattern-variables . stuff)
  (mtch stuff
        (pat) (pathize-pattern-variables pat "lv")

        (pat path)
        (cond
         ((pair? pat) (append (pathize-pattern-variables (car pat) (++ path "a"))
                              (pathize-pattern-variables (cdr pat) (++ path "d"))))
         ((constant? pat) '())
         ((symbol? pat) (list (cons pat (->symbol path))))
         (#t (err pathize-pattern-variables stuff)))))

(define (subst-variables body ass)
  (cond
   ((pair? body) (cons (subst-variables (car body) ass)
                       (subst-variables (cdr body) ass)))
   ((constant? body) body)
   ((symbol? body) (let ((v (assoc body ass)))
                     (if (eq? v #f)
                         body
                         (cdr v))))
   (#t (err subst-variablest body ass))))

;(tracefun pathize-pattern-variables pathize-clause subst-variables pathize-clause)

;(tracefun clause->question-path merge-question-paths)

(define (question-tree-fun->scheme blah)
  (mtch blah
        ('Fun name tree)
        `(define (,name lv) ,(question-tree->scheme tree))))

(define (question-tree->scheme tree)
  (mtch tree
        ((('Question q) then) . else)
        `(if ,(question->scheme q)
             ,(question-tree->scheme then)
             ,(question-tree->scheme else))

        ()
        '()

        ((('Body b) (())))
        b))

(define (question->scheme q) q)

;(tracefun question-tree->scheme question-tree-fun->scheme)

;;    (cond
;;     ((pair? arg)
;;      (let ((car-path (++ var-path "a"))
;;            (cdr-path (++ var-path "d")))
;;        `(if (pair? ,var-path)
;;             (let ((,car-path (car ,var-path))
;;                   (,cdr-path (cdr ,var-path)))
;;               ,(clause->let (car arg) (clause-let (cdr arg) body cdr-path) car-path)))))
;;     ((ctor? arg)
;;      `(if (eq? ,var-path ',arg)
;;           ,body

(define (tlfs->rules tlfs)
  (let ((clause-groups (group-by fun-name tlfs)))
    (map (lambda (group)
           (mtch group
                 (name funs) `(Fun ,name ,(map fun->clause funs))))
         clause-groups)))

(define (hen-run file)
  (let ((rules (tlfs->rules (read-objects file))))
    ;(shew rules)
    (let ((question-trees (map fun->question-trees rules)))
      (shew question-trees))))
;    (shew (map question-tree-fun->scheme (map fun->question-trees rules)))))
