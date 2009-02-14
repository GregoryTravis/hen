(import
 (ffi "ref.c"))

(doo
 ref (create-int-ref 10)
 val (read-int-ref ref)
 _ (shew val)
 _ (write-int-ref ref 20)
 val (read-int-ref ref)
 _ (shew val)
 _ (destroy-int-ref ref))
