(def fact2
     (/./.
      (/. (0) 1)
      (/. (n) (* n (fact2 (- n 1))))))
(def main
     (/. () (fact2 10)))
