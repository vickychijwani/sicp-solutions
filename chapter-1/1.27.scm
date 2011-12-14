;;; (fermat-test) takes a positive integer n and returns #t if (expt a n) is
;;; congruent to a modulo n for every positive a < n, else returns #f
;;;
;;; Carmichael numbers tested: 561, 1105, 1729, 2465, 2821, 6601
;;; (Carmichael numbers are those that fool the Fermat test, i.e., if x is a
;;; Carmichael number, then (expt a x) is congruent to a for every a < x, yet x
;;; is not prime)

(define (fermat-test n)
  (define (square x)
    (* x x))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))))
  (define (try-it a n)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (try-it (+ a 1) n))
          (else #f)))
  (try-it 1 n))
