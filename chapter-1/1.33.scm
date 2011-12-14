;;; recursive higher order procedure for filtered accumulation

(define (filtered-accumulate filter? combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter? (term a))
         (combiner (term a)
                   (filtered-accumulate filter? combiner null-value term
                                        (next a) next b)))
        (else (filtered-accumulate filter? combiner null-value term
                                   (next a) next b))))

;;; a. the sum of the squares of prime numbers in the interval [a,b]
;;; the prime? procedure is taken from 1.23.scm
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

(define (sum-square-prime a b)
  (define (sqrt-is-prime? n)
    (prime? (sqrt n)))
  (define (square x) (* x x))
  (define (inc x) (+ x 1))
  (filtered-accumulate sqrt-is-prime? + 0 square a inc b))

;;; b. the product of all positive integers less than n that are co-prime to n,
;;; i.e., product of all +ve i < n such that GCD(i,n) = 1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-co-primes n)
  (define (co-prime-to-n? x)
    (= 1 (gcd x n)))
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (filtered-accumulate co-prime-to-n? * 1 identity 1 inc n))