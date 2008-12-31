(fun (create-int-ref value) (Command 'create-int-ref-impl ($ value)))
(fun (read-int-ref ref) (Command 'read-int-ref-impl ($ ref)))
(fun (write-int-ref ref value) (Command 'write-int-ref-impl ($ ref value)))
(fun (destroy-int-ref ref) (Command 'destroy-int-ref-impl ($ ref)))
