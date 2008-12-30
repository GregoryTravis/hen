(fun (create-int-ref value k) (X 'create-int-ref-impl value k))
(fun (read-int-ref ref k) (X 'read-int-ref-impl ref k))
(fun (write-int-ref arg k) (X 'write-int-ref-impl arg k))
(fun (destroy-int-ref ref k) (X 'destroy-int-ref-impl ref k))
