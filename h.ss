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

(define (tuu e)
  (mtch e
        ('P ('K 'Integer) ('P ('Prim x) 'Nil)) x
        ('P ('K 'String) ('P ('Prim x) 'Nil)) x
        ('P a b) (cons (tuu a) (tuu b))
        ('K x) x
        ('V x) (err 'tuu e)
        'Nil '()))

(define (try-funs e funs)
  (if (null? funs)
      (err 'fail e)
      (mtch (try-fun e (car funs))
            'Ono (try-funs e (cdr funs))
            (Yay result) result)))

(define (try-fun e fun)
  (mtch fun
        ('Fun pat body)
        (let ((bindings (pat-match pat e)))
          (if (any? (map (feq? #f) bindings))
              'Ono
              `(Yay ,(rewrite-body body bindings))))))

(define (pat-match pat e)
  (mtch (list pat e)
        (('P a b) ('P c d)) (append (pat-match a c) (pat-match b d))
        (('V x) y) `((,x . ,y))
        (('K a) ('K b)) (if (== a b) '() #f)
        (('Prim a) ('Prim b)) (if (== a b) '() #f)
        ('Nil 'Nil) '()))

(define (rewrite-body body bindings)
  (mtch body
        ('P a b) `(P ,(rewrite-body a bindings) ,(rewrite-body b bindings))
        ('V x) (lookup x bindings)
        ('K x) body
        ('Prim x) body
        'Nil body))

(define (evaluated? e)
  (mtch e
        ('P ('K x) b) (ctor? x)
        ('K x) #t
        ('V x) (err 'evaluated e)
        ('Prim x) #t
        'Nil #t))

(define (evl-rewrite e funs)
  (if (evaluated? e)
      e
      (try-funs e funs)))

(define (evl-children e funs)
  (mtch e
        ('P a b) (p-map ($ evl _ funs) e)
        ('K x) e
        'Nil e
        ('Prim x) e))

(define (evl-step e funs)
  (evl-rewrite (evl-children e funs) funs))

(define (evl-drive e funs)
  (let ((r (evl-step e funs)))
    (if (evaluated? r)
        r
        (evl-drive r funs))))

(define (evl e funs)
  (evl-drive e funs))

(define (evl-top e funs)
  (display "+ ")
  (shew (tuu e))
  (shew (tuu (evl e funs))))

;(tracefun try-funs try-fun pat-match rewrite-body)
;(tracefun evl evl-drive evl-step evl-children evl-rewrite)

(define (run-src forms)
  (mtch (group-by-preds (list fun? ftrue) forms)
        (funs tles)
        (map ($ evl-top _ funs)
             (if (member? 'main (map fun-name funs))
                 (snoc tles '(main))
                 tles))))

(define (hen-run file)
  (run-src (uut (read-objects file))))
