(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
   (* 3 (- 6 2) (- 2 7)))


(define (square a) (* a a))
(define (sum-squares a b) (+ (square a) (square b)))

(define (lol a b c) (cond
                     ((and (<= a b) (<= a c)) (sum-squares b c))
                     ((and (<= b a) (<= b c)) (sum-squares a c))
                     (else (sum-squares a b))
                     ))

(lol 2 3 1)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 1 2)

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; sqrt stuff start

;; always evaluates both branches of the clause so can lead to infinite loops
(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (<
   (/ (abs (- (improve guess x) guess)) guess)
   0.0000000000001))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

;; cube root
(define (cube-root-iter guess x guesses)
  (if (good-enough-cube? guess x)
      guesses
      (cube-root-iter (improve-cube guess x) x (append (list guess) guesses))))

(define (good-enough-cube? guess x)
  (<
   (/ (abs (- (improve-cube guess x) guess)) guess)
   0.000000000000000001))

;; which of these methods takes less iterations?
;; I bet it's just the avg - you were wrong. Why?
(define (improve-cube guess x)
  (/
   (+ (/ x (square guess)) (* 2 guess))
   3))

;; (define (improve-cube guess x)
;;   (average (/ x (square guess)) guess))

(define (cube-root x)
  (cube-root-iter 1.0 x '()))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 2)
        ((= kinds-of-coins 3) 5)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


(define (f n)
  (define (f-mul x) (* x (f (- n x))))
  (if (< n 3)
      n
      (+ (f-mul 1) (f-mul 2) (f-mul 3))))

(define (f-it n)
  (define (iter count a b c)
    (if (= count n)
        a
        (iter
         (+ count 1)
         b
         c
         (+  (* 3 a) (* 2 b) c)
         )))
  (if (< n 0)
      n
      (iter 0 0 1 2)))

(define (pascals row col)
  (if (or  (= row col) (= col 1))
      1
      (+
       (pascals (- row 1) (- col 1))
       (pascals (- row 1) col))))

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))

  (iter 1 b n))

(define (double x) (* 2 x))
(define (half x) (/ x 2))

(define (mul a b)
  (trace-define (iter acc a b)
    (cond ((= b 0) acc)
          ((even? b) (iter acc (double a) (half b)))
          (else (iter (+ acc a) a (- b 1)))))

  (iter 0 a b))

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))  ; compute p′
                   (+ (square q) (* 2 p q)) ; compute q′
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


(trace-define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (runtime) (time-nanosecond (current-time)))
(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime))
  (newline))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes cur end)
  (cond ((>= cur end))
        ((even? cur) (search-for-primes (+ cur 1) end))
        (else (begin
                (timed-prime-test cur)
                (search-for-primes (+ cur 2) end)))))

;; 1009 *** 2933
;; 1013 *** 3283
;; 1019 *** 2864
;;
;; 10007 *** 2864
;; 10009 *** 3213
;; 10037 *** 3283
;;
;; 100003 *** 8171
;; 100019 *** 7542
;; 100043 *** 8102

;; carmichael numbers
;; 561, 1105, 1729, 2465, 2821, and 6601.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times prime-test)
  (cond ((= times 0) #t)
        ((prime-test n) (fast-prime? n (- times 1) prime-test))
        (else #f)))

(define (fast-prime-fermat? n times) (fast-prime? n times fermat-test))

(fast-prime-fermat? 561 10) ; => #t
(fast-prime-fermat? 1105 10); => #t
(fast-prime-fermat? 1729 10); => #t
(fast-prime-fermat? 2465 10); => #t
(fast-prime-fermat? 2821 10); => #t
(fast-prime-fermat? 6601 10); => #t

;;  ^^ these should be false but the carmichael numbers fool fermat's prime test

(define (!= a b) (not (= a b)))
(define (miller-rabin-test n)
  (trace-define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let*
               ((result (expmod base (/ exp 2) m))
                (square-mod-m (remainder (square result) m)))
             (if (and (!= result 1) (!= result (- n 1)) (= square-mod-m 1))
                 0                      ; non-trivial square root found
                 square-mod-m)))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-miller-rabin? n times) (fast-prime? n times miller-rabin-test))

(import (srfi :64))

(test-begin "miller rabin primality testing")

;; carmichaels
(test-assert (not (fast-prime-miller-rabin? 561 10))) ; => #f
(test-assert (not (fast-prime-miller-rabin? 1105 10))) ; => #f
(test-assert (not (fast-prime-miller-rabin? 1729 10))) ; => #f
(test-assert (not (fast-prime-miller-rabin? 2465 10))) ; => #f
(test-assert (not (fast-prime-miller-rabin? 2821 10))) ; => #f
(test-assert (not (fast-prime-miller-rabin? 6601 10))) ; => #f

;; other tests
(test-assert (fast-prime-miller-rabin? 2 10))  ; => #t
(test-assert (fast-prime-miller-rabin? 3 10))  ; => #t
(test-assert (not (fast-prime-miller-rabin? 4 10)))  ; => #f
(test-assert (fast-prime-miller-rabin? 5 10))  ; => #t
(test-assert (not (fast-prime-miller-rabin? 6 10)))  ; => #f
(test-assert (not (fast-prime-miller-rabin? 9 10)))  ; => #f
(test-assert (not (fast-prime-miller-rabin? 15 10))) ; => #f

(test-assert (fast-prime-miller-rabin? 1009 10)) ; => #t
(test-assert (fast-prime-miller-rabin? 1013 10)) ; => #t
(test-assert (fast-prime-miller-rabin? 100043 10))

(test-end "miller rabin primality testing")
(test-runner-reset (test-runner-get))


(trace-define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define y0 a)
  (define yn (+ a (* n h)))
  (define (term x) (* 2 (f x)))
  (define (next x) (+ x h))
  (define y-all (sum term (+ a h) next b))
  (define y-odd (sum term (+ a h) (lambda (x) (next (next x))) b))

  (* (/ h 3) (+ y0 y-all y-odd yn)))

(define (cube x) (* x x x))


(trace-define (sum term a next b)
  (trace-define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))

  (iter a 0))

(trace-define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(trace-define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))

  (iter a 1))

(trace-define (accumulate f base-case)
  (trace-define (acc term a next b)
    (if (> a b)
        base-case
        (f (term a) (acc term (next a) next b))))
  acc)

(trace-define (accumulate f base-case)
  (trace-define (acc term a next b)
    (trace-define (iter a result)
      (if (> a b)
          result
          (iter (next a) (f (term a) result))))
    (iter a base-case))
  acc)

(trace-define sum (accumulate + 0))
(trace-define product (accumulate * 1))

(define (identity x) x)
(define (factorial n)
  (define (add1 x) (+ x 1))
  (product identity 1 add1 n))

(trace-define (pi n)
  (define (numerator_series x) (+ 2 (* 2 (quotient x 2))))
  (define (denominator_series x) (+ 3 (* 2 (quotient x 2))))
  (define numerator (product numerator_series 1 add1 n))
  (define denominator (product denominator_series 0 add1 (- n 1)))

  (* 4 (/ numerator denominator)))

(define (filtered-accumulate f null-case term filter next)
  (define (filtered-term x)
    (if (filter x)
        (term x)
        null-case))
  (define (acc a b)
    (if (> a b)
        null-case
        (f (filtered-term a) (acc (next a) b))))
  acc)

(define (sum-squares-of-primes a b)
  ((filtered-accumulate + 0 square prime? add1) a b))

(define (product-of-coprime-smaller-naturals n)
  (define (is-coprime? x)
    (= (gcd x n) 1))
  ((filtered-accumulate * 1 identity is-coprime? add1) 1 (- n 1)))

(define tolerance 0.00001)
(trace-define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (trace-define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-to-x-solution)
  (fixed-point
   (lambda (x) (average x (/ (log 1000) (log x))))
   1.1))

(trace-define (cont-frac f g k)
  (trace-define (step i)
    (if (> i k)
        0
        (/ (f i) (+ (g i) (step (add1 i))))))
  (step 1))


(trace-define (cont-frac f g k)
  (trace-define (iter i result)
    (if (<= i 0)
        result
        (iter (- i 1) (/ (f i) (+ (g i) result)))))
  (iter k 0))

(define (e-2)
  (cont-frac
   (lambda (i) 1.0)
   (lambda (i)
     (cond
      ((= i 1) 1.0)
      ((= i 2) 2.0)
      ((= i 3) 1.0)
      ((= i 4) 1.0)
      ((= i 5) 4.0)
      ((= i 6) 1.0)
      ((= i 7) 1.0)
      ((= i 8) 6.0)
      ((= i 9) 1.0)
      ((= i 10) 1.0)
      ((= i 11) 8.0)
      (else (error #f "k too high rip"))))
   11))

(define (tan-cf x k)
  (cont-frac
   (lambda (i) (if (= i 1) x (- (square x))))
   (lambda (i) (- (* 2 i) 1.0))
   k))
