(load "lib.ss")
(load "util.ss")
(load "primitives.ss")

(define (boom name) (lambda (x) (err 'boom name x)))

(define (fun? e) (mtch e ('fun a b) #t _ #f))
(define (fun-name e) (mtch e ('fun (name . args) b) name))

;; Separate funs from top-level expressions.  Create _tle that runs
;; the top-level expressions, and _main which runs _tle and, if it
;; exists, main.
(define (tlfs->defs forms)
  (mtch (group-by-preds (list fun? true-pred) forms)
        (funs tles)
        (append funs
                (if (null? tles) '() `((fun (_tle) (begin ,@tles))))
                (list
                 (if (has-main? forms)
                     `(fun (_main) (begin (_tle) (main)))
                     `(fun (_main) (_tle)))))))

;; Add main if there isn't one, and add _main which calls _tles and
;; main.  This is done before funs->defs.
(define (has-main? tlfs) (any? (map (lambda (x) (and (fun? x) (eq? (caadr x) 'main))) tlfs)))

(define (hen-run file)
  (shew (tlfs->defs (read-objects file))))

;(tracefun simplify-patterns patterns->conditionals ->scheme)
;(tracefun def->scheme gort)
