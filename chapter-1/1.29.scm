;;; Simpson's Rule for approximation of the integral of a function f between a
;;; and b:
;;; (h/3) * (y(0) + 4*y(1) + 2*y(2) + 4*y(3) + 2*y(4) + ... + 4*y(n-1) + y(n))
;;; where h = (b-a)/n for some even integer n, and y(k) = f(a+kh) (increasing n
;;; increases the accuracy of the approximation)
;;;
;;; comparison of various approximations for (integral cube 0 1):
;;; - exact value = 0.25
;;; - by the first method (end of section 1.3.1)
;;;     (integral cube 0 1 0.01) = 0.24998750000000042
;;;     (integral cube 0 1 0.001) = 0.249999875000001
;;; - by Simpson's Rule
;;;     (integral cube 0 1 100.0) = 0.24666666666666687
;;;     (integral cube 0 1 1000.0) = 0.24966666666666754

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define (simpson-h)
    (/ (- b a) n))
  ;; t = a + kh; (simpson-k t) returns k given t
  (define (simpson-k t)
    (round (/ (- t a) (simpson-h))))
  ;; the (term) function to be applied on the current term varies; it is either
  ;; f(x), 2*f(x), or 4*f(x) depending on the value of (simpson-k t)
  (define (simpson-term t)
    (define k (simpson-k t))
    (cond ((or (= k 0) (= k n)) (f t))
          ((even? k) (* 2 (f t)))
          (else (* 4 (f t)))))
  (define (simpson-next a)
    (+ a (simpson-h)))
  (if (even? n)
      (* (/ (simpson-h) 3)
         (sum simpson-term a simpson-next b))
      (display "n must be even!\n")))