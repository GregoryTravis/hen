(X 'create-ref 10
   (/. (r) (X 'read-ref r
              (/. (x) (X 'shew  x
                         (/. (x) (X 'write-ref ($ r 20)
                                    (/. (x) (X 'read-ref r id)))))))))
