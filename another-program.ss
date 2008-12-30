(create-int-ref 10
                (/. (r) (read-int-ref r
                                      (/. (x) (shew x
                                                    (/. (dummy) (write-int-ref ($ r 20)
                                                                               (/. (dummy) (read-int-ref r
                                                                                                         (/. (x) (shew x
                                                                                                                       (/. (dummy) (destroy-int-ref r id)))))))))))))
