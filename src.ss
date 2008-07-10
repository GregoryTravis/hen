(fun ('foo a b) ('bar b b))
('foo 10 20)
('bar 10 20)
('foo 20 10)
('bar 20 10)

(fun ('boo) ('fark))
(fun ('boo a . d) ('boo . d))
('boo 1 2 3)

(fun ('gig ()) ())
(fun ('gig (a . d)) (('gorp a a) ('gig d)))
('gig (1 2 3 4))

(fun ('tak ()) ())
(fun ('tak ('cons a d)) ('cons ('harf a a) ('tak d)))
('tak ('cons 1 ('cons 2 ())))

(fun ('yep a) ('double a a))
(fun ('map f ()) ())
(fun ('map f ('cons a d)) ('cons (f a) ('map f d)))
('map 'yep ('cons 1 ('cons 2 ())))
