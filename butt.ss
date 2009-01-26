(foreign "butt" "butt.o butt.impl.o" "")

(doo
 ref (create-butt 10)
 val (read-butt ref)
 _ (shew val)
 _ (write-butt ref 20)
 val (read-butt ref)
 _ (shew val)
 _ (destroy-butt ref))
