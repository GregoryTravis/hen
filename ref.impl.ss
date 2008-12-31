(define (create-int-ref-impl v) (opaque (box v)))
(define (read-int-ref-impl r) (unbox (opaque-val r)))
(define (write-int-ref-impl ref value) (begin (set-box! (opaque-val ref) value) 'Nil))
(define (destroy-int-ref-impl r) ''Nil)

(register-command 'create-int-ref-impl create-int-ref-impl)
(register-command 'read-int-ref-impl read-int-ref-impl)
(register-command 'write-int-ref-impl write-int-ref-impl)
(register-command 'destroy-int-ref-impl destroy-int-ref-impl)
