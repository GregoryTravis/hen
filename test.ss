(load "h.ss")

;(run-file "test-input.ss")

(define rules
  '(
    ((cton Foo ((var a) (const (opaque 20)) (var c)))
     (cton Bar ((var c) (var a))))))

(define targets
  '((cton Foo ((const (opaque 10)) (const (opaque 20)) (const (opaque 30))))
    (cton Foo ((const (opaque 10)) (const (opaque 20)) (const (opaque 300))))
    (cton Foo ((const (opaque 10)) (const (opaque 200)) (const (opaque 30))))
    (cton Foo ((const (opaque 10)) (const (opaque 20))))))

(shew (map ($ try-rules rules _) targets))

(map sus (car rules))
(map sus targets)
