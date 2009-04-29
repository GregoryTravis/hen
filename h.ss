(load "lib.ss")
(load "util.ss")
(load "primitives.ss")

;(define (fun-name e) (mtch 

(defn fun-name
  (('Fun (App name pat) body)) name)

(defn fun? (('Fun pat body)) #t _ #f)

;; -> Fun, K, I, P, Prim
(define (parse form)
  (mtch form
        ('fun pat body) `(Fun ,(parse pat) ,(parse body))
        (a . b) `(P ,(parse a) ,(parse b))
        () `Nil
        x (cond ((ctor? x) `(K ,x))
                ((symbol? x) `(I ,x))
                ((integer? x) `(P (K Integer) (P (Prim ,x) Nil)))
                ((string? x) `(P (K String) (P (Prim ,x) Nil)))
                (#t (err x)))))

(define (p-map f e)
  (mtch e
        ('P a b) `(P ,(p-map f a) ,(p-map f b))
        _ (f e)))

(define (identifiers->k-or-v e bound)
  (mtch e
        ('Fun pat body) (let ((pat (identifiers->k-or-v pat bound)))
                          `(Fun ,pat ,(identifiers->k-or-v body (append (pat-bindings pat) bound))))
        ('P ('I x) d) `(P ,(if (member? x bound) `(V ,x) `(K ,x)) ,(p-map ($ identifiers->k-or-v _ bound) d))
        ('P a d) (p-map ($ identifiers->k-or-v _ bound) e)
        ('I x) `(V ,x)
        ('K x) e
        ('Prim p) e
        'Nil e))

(define (pat-bindings pat)
  (mtch pat
        ('P a b) (append (pat-bindings a) (pat-bindings b))
        'Nil '()
;        ('I x) (list x)
        ('V x) (list x)
        ('K x) '()))

;; TODO: top-level list of forms -> P-ified?
(define (uut forms)
  (map ($ identifiers->k-or-v _ '()) (map parse forms)))

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
  (run-src (uut (read-objects file))))
;  (shew (tlfs->defs (read-objects file))))

;(tracefun simplify-patterns patterns->conditionals ->scheme)
;(tracefun def->scheme gort)
