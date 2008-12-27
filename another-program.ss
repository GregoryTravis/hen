(X 'create-int-ref 10
   (/. (r) (X 'read-int-ref r
              (/. (x) (X 'shew  x
                         (/. (x) (X 'write-int-ref ($ r 20)
                                    (/. (x) (X 'read-int-ref r
                                               (/. (x) (X 'shew  x
                                                          (/. (x) (X 'destroy-int-ref r id)))))))))))))
