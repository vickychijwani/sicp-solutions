;;; representing line segments in a plane

;;; points

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;; line segments

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (define (average a b)
    (/ (+ a b) 2))
  (let ((start-x (x-point (start-segment s)))
        (start-y (y-point (start-segment s)))
        (end-x (x-point (end-segment s)))
        (end-y (y-point (end-segment s))))
    (make-point (average start-x end-x) (average start-y end-y))))