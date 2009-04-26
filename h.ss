(load "lib.ss")
(load "util.ss")
(load "primitives.ss")

;(define (fun-name e) (mtch 

(defn fun-name
  (('Fun (App name pat) body)) name)

(defn fun? (('Fun pat body)) #t _ #f)

(define (parse form)
  (mtch form
        ('fun pat body) `(Fun ,(parse pat) ,(parse body))
        (a . b) (if (ctor? a)
                    `(Ctor ,a ,(map parse b))
                    `(App ,(parse a) ,(parse b)))
        x x))

(define (evl e funs)
  (shew 'evl e))

(define (run-src forms)
  (mtch (group-by-preds (list fun? ftrue) forms)
        (funs tles)
        (map ($ evl _ funs)
             (if (member? 'main (map fun-name funs))
                 (snoc tles '(main))
                 tles))))

(define (hen-run file)
  (run-src (map parse (read-objects file))))
;  (shew (tlfs->defs (read-objects file))))

;(tracefun simplify-patterns patterns->conditionals ->scheme)
;(tracefun def->scheme gort)
