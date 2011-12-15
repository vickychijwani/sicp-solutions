;;; The (good-enough?) test given in section 1.1.7 will not be very effective
;;; for very small numbers because at some point they will become comparable to
;;; fixed tolerance value (0.001).
;;;
;;; The (good-enough?) also will not be very effective for large numbers because
;;; of 2 reasons:
;;; 1. The approximation may take a lot of time to improve the guess to
;;;    the fixed tolerance value.
;;; 2. From http://eli.thegreenplace.net/2007/06/21/sicp-section-11/:
;;;    "Floating point numbers are usually represented with an exponent and a
;;;    mantissa, and when the exponent grows large, the quanta the number can
;;;    advance in become large. In our example, after a few iterations the
;;;    number 3.039737E8 was reached and from there the process entered
;;;    infinite recursion because (improve 3.039737E8) returns 3.039737E8
;;;    itself."

(define (sqrt x)
  (define (square x)
    (* x x))
  (define (good-enough? guess)
    (< (abs (- (/ (improve guess) guess) 1)) 0.001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess) x)))
  (sqrt-iter 1.0 x))