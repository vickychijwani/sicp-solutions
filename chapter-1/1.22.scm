;;; Computing power has grown much since SICP, 2nd edition was written,
;;; so it now takes less than a microsecond to check the primality of teeny
;;; numbers like 100,000. Therefore I had to test on larger numbers (starting
;;; from 10,000,000) to watch clearly the O(sqrt(n)) order of growth of the
;;; primality test.
;;;
;;; The answer to the exercise question asking whether the results are
;;; consistent with the predicted order of growth is a definite 'Yes'; the time
;;; taken for the computation seems to grow by about 3 (=~ sqrt(10)) times for
;;; every ten-fold increase in the input.

(define (prime? n)
  (define (square x)
    (* x x))
  (define (divides? divisor n)
    (= (remainder n divisor) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (define (report-prime n elapsed-time)
    (display n)
    (display " *** ")
    (display elapsed-time)
    (newline))
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime n (- (runtime) start-time))))
  (start-prime-test n (runtime)))

(define (search-for-primes a b)
  ;; first argument 'a' of current-is-prime? is assumed to be odd,
  ;; even numbers need not be checked
  (define (current-is-prime? a b)
    (timed-prime-test a)
    (search-for-primes (+ a 2) b))
  (if (<= a b)
      (if (odd? a)
       (current-is-prime? a b)
       (current-is-prime? (+ a 1) b))))