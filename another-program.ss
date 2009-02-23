(import
 (ffi "ref.c")
 (link "shew.impl.c"))

(doo
 ref (create-int-ref 10)
 val (read-int-ref ref)
 _ (shew val)
 _ (write-int-ref ref 20)
 val (read-int-ref ref)
 _ (shew val)
 _ (destroy-int-ref ref))

(doo
 ref (create-charp-ref "asdf")
 s (read-charp-ref ref)
 _ (shew s))
