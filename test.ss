(load "h.ss")

;(run-file "test-input.ss")

(define rules
  '(
    ((Foo a 20 c) (Bar c a))))

(define targets
  `((Foo 10 20 30)
    (Foo 10 20 300)
    (Foo 10 200 30)
    (Foo 10 20)))

(define more-exps
  '((app (const joe) ((cton Hoot ((const (opaque 10))))))
    (app (const joe) ((const (opaque 10))))))

(shew (map ($ try-rules (map (lambda (rule) (map unsugar rule)) rules) _) (map unsugar targets)))
(map usu (apply append rules))
(map usu targets)
(map sus more-exps)
