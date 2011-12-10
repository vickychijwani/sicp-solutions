;;; The 12 primes from ex 1.22 are (read the comment in 1.22.scm before this):
;;; >     10,000,000 :     10,000,019;     10,000,079;     10,000,103;
;;; >    100,000,000 :    100,000,007;    100,000,037;    100,000,039;
;;; >  1,000,000,000 :  1,000,000,007;  1,000,000,009;  1,000,000,021;
;;; > 10,000,000,000 : 10,000,000,019; 10,000,000,033; 10,000,000,061;
;;;
;;; After testing I observed that the speed of (prime?) went up only by about
;;; 1.5x, and NOT by 2x, as expected.

(define (prime? n)
  (define (square x)
    (* x x))
  (define (divides? divisor n)
    (= (remainder n divisor) 0))
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
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