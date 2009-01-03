(fun (vark)
     (doo
      _ (shew 100)))

(doo
 _ (shew 10)
 _ (vark)
 _ (shew 20))

(doo _ (shew 100) _ (shew 200))
(doo _ (shew 100) _ (doo _ (shew 3000)) _ (shew 200))

;; ;(CommandSeq (Command 'shew ($ 100)) (/. (r) (Command 'shew ($ 200))))

;; ;($ (shew 10))
