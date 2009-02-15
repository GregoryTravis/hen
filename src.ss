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

(doo
 ref (create-float-ref 10.0)
 val (read-float-ref ref)
 _ (shew val)
 _ (write-float-ref ref 20.0)
 val (read-float-ref ref)
 _ (shew val)
 _ (destroy-float-ref ref))

(doo
 char-ref (create-charp-ref "fbo")
 charp (read-charp-ref char-ref)
 _ (shew charp))
