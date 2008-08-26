(read-file "tst-readee.ss")

(shew 5 5 6)
(map 'shew [5 5 6])
(map shew (form->list (5 5 6)))
(shew [4 5])

(fun (blah x) (> x 10))
(fun (bluh x) (< x 10))

(grep 'blah [1 2 30 40 5])
(grep 'bluh [1 2 30 40 5])
(grep blah [1 2 30 40 5])
(grep bluh [1 2 30 40 5])
