;;; The 12 primes from ex 1.22 are (read the comment in 1.22.scm before this):
;;; >     10,000,000 :     10,000,019;     10,000,079;     10,000,103;
;;; >    100,000,000 :    100,000,007;    100,000,037;    100,000,039;
;;; >  1,000,000,000 :  1,000,000,007;  1,000,000,009;  1,000,000,021;
;;; > 10,000,000,000 : 10,000,000,019; 10,000,000,033; 10,000,000,061;
;;;

(define (prime? n)
  (define (square x)
    (* x x))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))))
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))
  (fast-prime? n 5000))

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