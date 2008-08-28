(define show-reductions #f)
(define show-normalizations #f)
(define show-counts-p #f)
(define counts-rws 0)

(define (show-counts)
  (display "[")
  (display counts-rws)
  (display " reductions]\n"))

(define (reset-counts)
  (set! counts-rws 0))

(define (tick-rw)
  (set! counts-rws (+ counts-rws 1)))

(define (top-rw-dumper rule e result)
  (if (not (eq? 'fail result))
      (begin
        ;(display "<== ")
        (lsb e)
        (display "\n==> ")
        (lsb result)
        (display "\n                       ** ")
        (lsb rule)
        (display "\n"))
  '()))

(define (normalize-dumper e rws result)
  (begin
    (lsb e)
    (display "\n==> ")
    (lsb result)
    (display "\n")))

(define (evl-dumper e rws result)
  (display "+ ")
  (sb e)
  (sb result)
  (display "\n"))

(define (top-rw-ticker rule t result)
  (if (not (eq? 'fail result))
      (tick-rw)
      '())
  result)

(define (evl-dumper-before e rws)
  (reset-counts)
  (display "+ ")
  (sb e))
(define (evl-dumper-after e rws result)
  (sb result)
  (show-counts)
  (display "\n"))

(if show-reductions (hook-with (args-and-result-hook top-rw-dumper) top-rw) '())
(if show-normalizations
    (hook-with (args-and-result-hook normalize-dumper) normalize)
    (hook-with (args-and-result-hook evl-dumper) evl))
(if show-counts-p
    (begin
      (hook-with (args-and-result-hook top-rw-ticker) top-rw)
      (hook-with (before-and-after-hook evl-dumper-before evl-dumper-after) evl))
    '())

