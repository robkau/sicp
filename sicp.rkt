#lang sicp

(#%provide sum-of-squares)
(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

(define (sum-of-two-biggest-squares a b c)
  (cond
    [(and (> b a) (> c a)) (sum-of-squares b c)]
    [(and (> a c) (> b c) (sum-of-squares a b))]
    [else (sum-of-squares a c)]))

;; Running (test 0 (p)) is:
;; infinite recursion when using applicative order evaluation
;; runs fine when using normal order evaluation
(define (p)
  (p))
(define (test x y)
  (if (= x 0) 0 y))

;; Newton's square root calculation
(#%provide sqrt-iter)
(define (sqrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess previous-guess)
  (< (abs (- guess previous-guess)) 0.000000001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

;; Exercise 1.6, defining if without a special form.
(define (new-if predicate then-clause else-clause)
  (cond
    [predicate then-clause]
    [else else-clause]))

(define (sqrt-iter-newif guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-newif (improve guess x) x)))
;; this is infinite recursion because of applicative order evaluation (keeps going down and evaluating the 'new-if' arguments). the special-form if only evaluates the matching predicate branch).

;; Exercise 1.8 Newton's method for cube roots.
(define (cube-root-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
      guess
      (cube-root-iter (improve-cube guess x) guess x)))

(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-root x)
  (cube-root-iter 1.0 0.0 x))

;; Exercise 1.10 Ackermann function
(define (A x y)
  (cond
    [(= y 0) 0]
    [(= x 0) (* 2 y)]
    [(= y 1) 2]
    [else (A (- x 1) (A x (- y 1)))]))

(define (f n)
  (A 0 n))
(define (g n)
  (A 1 n))
(define (h n)
  (A 2 n))

;; Exercise 1.11
;; A function f is defined by the rule that:
;; f(n)=n if n<3
;; f(n)=f(n-1) + 2*f(n-2) + 3*f(n-3) if n≥3
;; Write a procedure that computes f by means of a recursive process.
(define (111-recursive n)
  (cond
    [(< n 3) n]
    [else
     (+ (111-recursive (- n 1))
        (* 2 (111-recursive (- n 2)))
        (* 3 (111-recursive (- n 3))))]))
;; Write a procedure that computes f by means of an iterative process.
;; todo!

;; Exercise 1.12
;; Write a procedure that computes elements of Pascal’s triangle by means of a recursive process.
(define (pascal row col)
  (cond
    [(= row 1) 1]
    [(= col 1) 1]
    [(= row col) 1]
    [else
     (+ (pascal (- row 1) (- col 1))
        (pascal (- row 1) col))]))

(define (display-pascal-row n)
  (define (column-iter i)
    (display (pascal n i))
    (display "  ")
    (if (= i n)
        (newline)
        (column-iter (+ i 1))))
  (column-iter 1))

(define (display-pascal n)
  (define (display-pascal-iter i)
    (display-pascal-row i)
    (if (= i n)
        (newline)
        (display-pascal-iter (+ i 1))))
  (display-pascal-iter 1))

;; Exercise 1.16
;; Design a procedure for iterative exponentiation using log(n) steps
;; This version using recursion is O (logn) steps and O(logn) space.
(define (fast-expt b n)
  (cond
    [(= n 0) 1]
    [(even? n) (square (fast-expt b (/ n 2)))]
    [else (* b (fast-expt b (- n 1)))]))

;; This version using recursive iteration is o (logn) steps and o(1) space.
(define (fast-expt-iter b n)
  (define (fast-expt-iter-inner b n a)
    (cond
      [(= n 0) a]
      [(even? n)
       (fast-expt-iter-inner (square b) (/ n 2) a)]
      [else (fast-expt-iter-inner b (- n 1) (* b a))]))
  (fast-expt-iter-inner b n 1))

;; Exercise 1.17
;; Write fast multiplication algorithm using log(n) number of steps.
;; Assume double(n) and halve(n) can be used.
(define (fast-mul a b)
  (define (double x)
    (* x 2))
  (define (halve x)
    (/ x 2))
  (cond
    [(= b 0) 0]
    [(even? b) (fast-mul (double a) (halve b))]
    [else (+ a (fast-mul a (- b 1)))]))

;; Exercise 1.18
;; todo russian peasant multiplication using 2 above.

;; Exercise 1.19
;; Computing fibonnaci in log(n) steps
;; Using T(a,b) = (bq + aq + ap, bp + aq)
;; Where fibonnaci is special case of p = 0, q = 1
;; Then T(a,b) applied to itself twice (squaring) leads to
;; b(q^2 + p^2) + a(2qp + q^2) + a(q^2 + p^2)  ,  (b(p^2 + q^2) + a(2pq + q^2)
;; Which is the same form except p' can be defined as q^2+p^2  , and q' is 2pq + q^2
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond
    [(= count 0) b]
    [(even? count)
     (fib-iter a
               b
               (+ (square q) (square p)) ; compute p'
               (+ (* 2 p q) (square q)) ; compute q'
               (/ count 2))]
    [else
     (fib-iter (+ (* b q) (* a q) (* a p))
               (+ (* b p) (* a q))
               p
               q
               (- count 1))]))

;; Exercise 1.19 from memory again
;; Fib(n) = Fib(n-1) + Fib(n-2)
;; Fib(<= 0) = 0
;  Fib(1) = 1

;; Fibonnaci transformation is a general form of
;; T(a,b) = (bq + aq + ap), (bp + aq)
;; Where (p,q) = (0,1)

;; Applying T twice to get the squared operation still expressed in terms of a,b,p,q
;; q' = p^2 + 2qp     p' = p^2 + q^2

(define (fib-mem n)
  (define (fib-mem-iter a b p q count)
    (cond
      [(<= count 0)
       b] ;; finished enough iterations. return the computed result.
      [(not (even? count))
       (fib-mem-iter ;; need to decrement by one, standard fibonacci step.
        (+ (* b p) (* a p) (* a q))
        (+ (* b p) (* a q))
        p
        q
        (- count 1))]
      [else
       (fib-mem-iter ;; opportunity to apply squared fibonnaci transformation and cut n in half.
        a
        b
        (+ (square p) (square q)) ; p' calculated
        (+ (square q) (* 2 q p)) ; q' calculated
        (/ count 2))]))
  (fib-mem-iter 1 0 0 1 n))

;; Simple fibonnaci with recursion
(define (fib-slow n)
  (define (fib-iter-inner a b n)
    (cond
      [(= n 0) a]
      [else (fib-iter-inner (+ a b) a (- n 1))]))
  (fib-iter-inner 1 0 n))

;; Easy printing helper
(define (display-all . vs)
  (for-each display vs))

;; Compare fibonnaci implementations
(define (fib-combined n)
  (display-all (fib n) " " (fib-mem n) " " (fib-slow n)))

;; 1.2.5 Greatest Common Divisors

;; Iterative recursive process to calculate GCD of two integers
;; GCD(a,b) = GCD(b,r)
;; r = remainder(a,b)
(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

;; Finding smallest divisor of a number
(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2)
      (+ n 1)
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond
    [(> (square test-divisor) n) n] ; over maximum possible
    [(divides? test-divisor n)
     test-divisor] ; found smallest
    [else (find-divisor n (next test-divisor))] ; try next
    ))

(define (divides? a b)
  (= (remainder b a) 0))

(#%provide prime?)
(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp)
     (remainder (square (expmod base (/ exp 2) m)) m)]
    [else
     (remainder (* base (expmod base (- exp 1) m)) m)]))

;; Fermats little theorem
;; Probabilistic prime determination
;; (Chance for error is less than a cosmic ray flipping a bit during calculation, says the book)
;; A false positive number is a Carmichael number  (561, 1105, 1729, 2465, ...)
; Only 255 below 100,000,000
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(#%provide fast-prime?)
(define (fast-prime? n times)
  (cond
    [(= times 0) true]
    [(fermat-test n) (fast-prime? n (- times 1))]
    [else false]))

;; Exercise 1.21: Use the smallest-divisor procedure to find the result for 199, 1999, 19999
; (smallest-divisor 199) 199
; (smallest-divisor 1999) 1999
; (smallest-divisor 19999) 7

;; Exercise 1.22 Using runtime primitive to measure how logn to find primes.
;; timed-prime-test takes integer n , prints n, and if n is prime prints three asterisks and the runtime.
;; Write procedure search-for-primes to find the first 3 primes larger than n, testing odd numbers only. compare if 10,100,1000... follows sqrt(10) order of growth in runtime , matching number of calculation steps.
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (and (report-prime n (- (runtime) start-time)) #t)
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes n)
  (define (search-for-primes-iter n at)
    (cond
      [(>= at 3)]
      [(even? n) (search-for-primes-iter (+ n 1) at)]
      [else
       (if (timed-prime-test n)
           (search-for-primes-iter (+ n 2) (+ at 1))
           (search-for-primes-iter (+ n 2) at))]))
  (search-for-primes-iter n 0))
#|
   > (search-for-primes 10000000000)
   10000000019 *** 13889
   10000000033 *** 12805
   10000000061 *** 13236
   #t
   > (search-for-primes 100000000000)
   100000000003 *** 50850
   100000000019 *** 36199
   100000000057 *** 32890
   #t
   > (search-for-primes 1000000000000)
   1000000000039 *** 108846
   1000000000061 *** 103993
   1000000000063 *** 101746

   Conclusion: sqrt(10) is about 3 and each order of magnitude increases runtime by about 3x. So time matches number of operations performed.
|#

;; Exercise 1.23
;; smallest-divisor function from earlier does a lot of needless checking.
;; it can be improved to only check odd numbers, after '2' has been checked.
;; implement this change and check if results from ex1.22 are twice as fast.
;; old time:
;; > (search-for-primes 1000000000)
;; 1000000007 *** 15071
;; 1000000009 *** 3323
;; 1000000021 *** 5080
;; new time:
;; > (search-for-primes 1000000000)
;; 1000000007 *** 930
;; 1000000009 *** 932
;; 1000000021 *** 932
;; it is even more than twice as fast.

#|
   Exercise 1.24
   Modify the timed-prime-test procedure from ex1.22 to use fast-prime? (Fermat method) and compare the speed to prime?.
   Since fermat test has logn growth does it match expectations?


 prime? speed:
> (search-for-primes 100000000)
100000007 *** 2190
100000037 *** 254
100000039 *** 256
> (search-for-primes 1000000000)
1000000007 *** 1580
1000000009 *** 9625
1000000021 *** 2760

 fast-prime? speed with 5 iters:
> (search-for-primes 100000000)
100000007 *** 38
100000037 *** 39
100000039 *** 38
> (search-for-primes 1000000000)
1000000007 *** 16
1000000009 *** 16
1000000021 *** 14

- fast-prime? is from 5x to 500x faster.
- fast-prime? actually gets faster after adding a zero and computing bigger primes? Not sure why.
- fast-prime? can't compute larger values. unlike prime? version, because the (random) func limits the input size and fast-prime? uses (random).
|#

#|
Exercise 1.25
Can expmod function replace all its internal code by calling (remainder (fast-expt base exp) m) ?
|#
(define (expmod-1-25 base exp m)
  (remainder (fast-expt base exp) m))
;; Original version computed primes in the millions in milliseconds, this one hung for over 10 seconds.
;; This version computes the biggest numbers in full in memory, old version breaks it down into smaller calculations and never renders the biggest number in mem.

#|
Exercise 1.26
Why is this version of expmod replacing (square x) with (* x x) changing it from O(logn) to O(n)?
|#
(define (expmod-slow base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp)
     (remainder (* (expmod-slow base (/ exp 2) m)
                   (expmod-slow base (/ exp 2) m))
                m)]
    [else
     (remainder (* base (expmod-slow base (- exp 1) m))
                m)]))
;; Because both arguments to (* x x) are both evaluated on each recursive call.

(define (coprime? a b)
  (= (gcd a b) 1))

#|
Exercise 1.27
Demonstrate that the Carmichael numbers listed in Footnote 47 really do fool the Fermat test.
That is, write a procedure that takes an integer n and tests whether a^n, modulo n, is equal to 1, for every a<n,
and try your procedure on the given Carmichael numbers.
|#

;; The test cases already show that carmichael numbers fool the fast-prime Fermat test method.
;; This function returns #t if number is a carmichael number.

(#%provide carmichael-num?)
(define (carmichael-num? n)
  (define (carmichael-num-iter b)
    (cond
      [(= b n) #t]
      [(and (coprime? b n) (not (= (expmod b (- n 1) n) 1)))
       #f]
      [else (carmichael-num-iter (+ b 1))]))

  (cond
    [(< n 561) #f] ;; the first carmichael number is 561.
    [(prime? n)
     #f] ;; slow but needed because some primes fool this carmichael test.
    [else (carmichael-num-iter 2)]))

#|
Exercise 1.28
Miller-Rabin test.
One variant of the Fermat test that cannot be fooled is called
the Miller-Rabin test (Miller 1976; Rabin 1980). This starts
from an alternate form of Fermat’s Little Theorem, which states that:
 if n is a prime number and a is any positive integer less than n,
 then a raised to the (n-1) power, modulo n, is 1.

To test the primality of a number n by the Miller-Rabin test,
we pick a random number a<n and raise a to the (n-1)-st power modulo n using the expmod procedure.

However, whenever we perform the squaring step in expmod,
we check to see if we have discovered a “nontrivial square root of 1 modulo n,”
that is, a number not equal to 1 or n-1 whose square, modulo n, is equal to 1.

It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime.

It is also possible to prove that if n is an odd number that is not prime, then,
for at least half the numbers a<n,
computing a^n(-1) in this way will reveal a nontrivial square root of 1 modulo n.
(This is why the Miller-Rabin test cannot be fooled.)

Modify the expmod procedure to signal if it discovers a nontrivial square root of 1,
and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test.

Check your procedure by testing various known primes and non-primes.
Hint: One convenient way to make expmod signal is to have it return 0.
|#

(#%provide miller-rabin-test)
(define (miller-rabin-test n times)
  (cond
    [(= times 0) #t]
    [(miller-rabin-check n)
     (miller-rabin-test n (- times 1))]
    [else #f]))

(define (miller-rabin-check n)
  (define (try-it a)
    (not (= 0 (expmod-checked a (- n 1) n))))
  (try-it (+ 1 (random (- n 1)))))

(define (remainder-square-checked x m)
  (if (and (not (or (= x 1) (= x (- m 1))))
           (= (remainder (* x x) m) 1))
      0
      (remainder (* x x) m)))

(define (expmod-checked base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp)
     (remainder-square-checked
      (expmod-checked base (/ exp 2) m)
      m)]
    [else
     (remainder (* base (expmod-checked base (- exp 1) m))
                m)]))

;; Note: lots of internet posts pointing out the description from the book is wrong about the miller-rabin 'witneses'.

(#%provide sum-recursive)
(define (sum-recursive term a next b)
  (if (> a b)
      0
      (+ (term a) (sum-recursive term (next a) next b))))

(define (inc n)
  (+ n 1))

(#%provide cube)
(define (cube a)
  (* a a a))

(#%provide sum-cubes)
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x)
  x)

(#%provide sum-integers)
(define (sum-integers a b)
  (sum identity a inc b))

(#%provide pi-sum)
(define (pi-sum a b sumf)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sumf pi-term a pi-next b))

(#%provide integral)
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;; Exercise 1.29 Simpson's Rule
;; Simpsons rule is a more accurate form of integrating than the function above.
;; The integral of f between a and b is approximated as:
;; (h/3)[y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2*yn-2 + 4*yn-1 + yn]
;; where h = (b-a)/n
;; where n is an even integer
;; where yk = f(a+k*h)
;; Compare results with the integral function above.
(#%provide simpson-integral)
(define (simpson-integral f a b n)
  (define h (/ (- b a) (* 1.0 n))) ;; h = (b-a)/n
  (define (coeff k)
    (cond
      [(= k 0) 1]
      [(= k n) 1]
      [(even? k) 2]
      [else 4]))
  (define (yk
           k) ;; yk = f(a+k*h) * coeff(k) ;; coeff stuffed in here since it also depends on k
    (* (coeff k) (f (+ a (* h k)))))

  (cond
    [(< n 2) -1] ;; n must be at least 2
    [(not (even? n)) -1] ;; n must be even
    [else
     (* (/ h 3) ;; perform the sum.
        (sum yk 0 inc n ;; todo n or n+1 or n-1?
             ))]))

(#%provide some-polynomial)
(define (some-polynomial x)
  (+ (fast-expt-iter x 7)
     (* -44 (fast-expt-iter x 5))
     (* 0.17 (fast-expt-iter x 2))
     (* (/ 1. 17) x)))

;; Exercise 1.30
;;  The sum procedure above generates a linear recursion.
;;  The procedure can be rewritten so that the sum is performed iteratively.
;;  Show how to do this by filling in the missing expressions in the following definition:

(#%provide sum)
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; more efficient than sum-recursive, no need to build and unwind a big callstack since all state is passed in each call.

;; Exercise 1.31
;; The sum procedure is only the simplest of a vast number of similar abstractions that
;; can be captured as higher-order procedures.
;; Write an analogous procedure called product that returns the product of the values of a function at points over a given range.
;; Show how to define factorial in terms of product
;; Also use product to compute approximations to π using the formula
;;  π/4 = 2 ⋅ 4 ⋅ 4 ⋅ 6 ⋅ 6 ⋅ 8 ⋅ ⋯
;;         --------------------------
;;         3 ⋅ 3 ⋅ 5 ⋅ 5 ⋅ 7 ⋅ 7 ⋅ ⋯

;; todo test me
(#%provide product)
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 0))

;; todo test me
(define (factorial n)
  (product identity 0 inc n))

(define (pi-frac-estimate n)
  (define (upper-digit n)
    ;; 2 4 4 6 6 8 8 ...
    2)
  (define (lower-digit n)
    ;; 3 3 5 5 7 7   ...
    3)

  (* 4
     (/ (product upper-digit 0 inc n)
        (product lower-digit 0 inc n))))

(define (pi-frac-estimate-other n)
  (define (digit-frac n)
    (/ (upper-digit n) (lower-digit n)))

  (define (upper-digit n)
    ;; 2 4 4 6 6 8 8 ...
    2)
  (define (lower-digit n)
    ;; 3 3 5 5 7 7   ...
    3)

  (* 4 (product digit-frac 0 inc n)))

;; If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
(define (product-recursive term a next b)
  ;;;;todo
  0)

;; ============================================
;; Chapter 2: Building Abstractions with Data
;; ============================================


;; Basic operations over rationals given n1/d1 and n2/d2 as two rationals

;; (n1d2 + n2d1) / d1d2
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;; (n1d2 - n2d1) / d1d2
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;; n1n2/d1d2
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))


;; n1d2/d1n2
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

;; equality: n1/d1 = n2/d2 , iff n1d2=n2d1
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;; constructor and accessors for rationals, represented as a pair
(define (make-rat-simple n d) (cons n d))   ;; does not reduce to gcds
(define (make-rat-gcd n d)  ;; reduces to gcds
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(define (one-half)
  (make-rat 1 2))

(define (one-third)
  (make-rat 1 3))



;;;;;;;;;;;;;;
;; Exercise 2.1:
;; Define a better version of make-rat that handles both positive and negative arguments.
;; make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive.
;; And if the rational number is negative, only the numerator is negative.
;;;;;;;;;;;;;;

(define (make-rat n d) ;; reduces to gcds and handles negative signs properly
  (let ((g (gcd n d))) ;; note: changed the previous 'gcd' function to return abs value to make this work.
    (let ((n-adjusted (if (< d 0) (- n) n))
          (d-adjusted (if (< d 0) (- d) d)))
      (cons (/ n-adjusted g) (/ d-adjusted g)))))


;; Export functions for testing
(#%provide add-rat sub-rat mul-rat div-rat equal-rat?
           make-rat make-rat-gcd make-rat-simple numer denom
           one-half one-third print-rat
           gcd
           make-point x-point y-point print-point
           make-segment start-segment end-segment midpoint-segment segment-length
           make-rect-naive left-seg-naive bottom-seg-naive right-seg-naive top-seg-naive
           perimeter-naive area-naive
           make-rect rect-origin rect-width rect-height rect-angle area perimeter
           cons-alt car-alt cdr-alt
           cons-intpow car-intpow cdr-intpow count-divisions
           zero-church one-church two-church three-church add-1-church
           make-interval lower-bound upper-bound add-interval sub-interval mul-interval div-interval
           interval-width)




;;;;;;;;;
;; Exercise 2.2:
;; Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point.
;; Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points.
;; Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate.
;; Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation.
;; Finally, using your selectors and constructors, define a procedure midpoint-segment that takes
;;   a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints).
;; To try your procedures, you’ll need a way to print points.
;;;;;;;;;


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))


(define (avg a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let ((xm (avg (x-point (start-segment s)) (x-point (end-segment s)))))
    (let ((ym (avg (y-point (start-segment s)) (y-point (end-segment s)))))
      (make-point xm ym)
      )))




;;;;;;;;;
;; Exercise 2.3:
;; Implement a representation for rectangles in a plane. (Hint: You might want to make use of Exercise 2.2 .)
;; In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle.
;; Now implement a different representation for rectangles.
;; Can you design your system with suitable abstraction barriers so that the same perimeter and area procedures will work using either representation?


;; naive representation, a rectangle is 4 lines
;; returns nested tuples in order of (left line, bottom line, right line, top line)
;; does not validate the lines actually form a rectangle with perpendicular angles!
(define (make-rect-naive ll bl rl tl)
  (cons ll (cons bl (cons rl tl)))
  )
(define (left-seg-naive rect)
  (car rect))

(define (bottom-seg-naive rect)
  (car (cdr rect)))

(define (right-seg-naive rect)
  (car (cdr (cdr rect))))

(define (top-seg-naive rect)
  (cdr (cdr (cdr rect))))

(define (segment-length seg)
  (let ((dx (- (x-point (end-segment seg))
               (x-point (start-segment seg)))))
    (let ((dy (- (y-point (end-segment seg))
                 (y-point (start-segment seg)))))
      (sqrt (+ (square dx) (square dy))))))

(define (perimeter-naive rect)
  (+ (segment-length (left-seg-naive rect)) (segment-length (bottom-seg-naive rect)) (segment-length (right-seg-naive rect)) (segment-length (top-seg-naive rect)))
  )

(define (area-naive rect)
  (* (segment-length (left-seg-naive rect)) (segment-length (bottom-seg-naive rect)))
  )


;; Second implementation.
;; rectangle is represented as: (origin point , width, length, angle of rotation)
;; Constructor
(define (make-rect origin-pt width height angle)
  (cons origin-pt (cons width (cons height angle))))

;; Selectors
(define (rect-origin rect) (car rect))
(define (rect-width rect) (car (cdr rect)))
(define (rect-height rect) (car (cdr (cdr rect))))
(define (rect-angle rect) (cdr (cdr (cdr rect))))

(define (area rect)
  (* (rect-width rect) (rect-height rect)))

(define (perimeter rect)
  (* 2 (+ (rect-width rect) (rect-height rect))))



;;;;;;;;;;;;
;; Exercise 2.4
;; Here is an alternative procedural representation of pairs.
;; For this representation, verify that (car (cons x y)) yields x for any objects x and y.

(define (cons-alt x y)
  (lambda (m) (m x y)))

(define (car-alt z)
  (z (lambda (p q) p)))

;; What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the substitution model of 1.1.5.)
;;;;;;;;;;;;

(define (cdr-alt z)
  (z (lambda (p q) q))
  )





;;;;;;;;;;;;
;; Exercise 2.5
;; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations,
;; if we represent the pair a and b as the integer that is the product 2^a * 3^b.
;; Give the corresponding definitions of the procedures cons, car, and cdr.


(define (cons-intpow a b)
  (* (fast-expt-iter 2 a) (fast-expt-iter 3 b))
  )


;; so repeatedly divide by 2 to find the 2 factor, and repeatedly divide by 3 to find the 3 factor.
(define (count-divisions n divisor)
  (define (count-divisions-iter n divisor count)
    (if (= (remainder n divisor) 0)
        (count-divisions-iter (/ n divisor) divisor (+ count 1))
        count))
  (count-divisions-iter n divisor 0)
  )



(define (car-intpow p)
  (count-divisions p 2)
  )

(define (cdr-intpow p)
  (count-divisions p 3)
  )




;;;;;;;;;;;;;;;;;
;; Exercise 2.6
;; In case representing pairs as procedures wasn’t mind-boggling enough, consider that, in a language that can manipulate procedures,
;; we can get by without numbers (at least insofar as nonnegative integers are concerned).
;; By implementing 0 and the operation of adding 1 as
(define zero-church (lambda (f) (lambda (x) x)))

(define (add-1-church n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;; This representation is known as Church numeral’s, after its inventor, Alonzo Church, the logician who invented the λ-calculus.
;; Define one and two directly (not in terms of zero and add-1).
;; (Hint: Use substitution to evaluate (add-1 zero)).
;; Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).
(define one-church (lambda (f) (lambda (x) (f x))))
(define two-church (lambda (f) (lambda (x) (f(f x)))))
(define three-church (lambda (f) (lambda (x) (f(f(f x))))))




;;;;;;;;;;;;;;
;; Exercise 2.7
;; Alyssa P. Hacker is designing a system to help people solve engineering problems.
;; One feature she wants to provide in her system is the ability to manipulate inexact quantities (such as measured parameters of physical devices)
;; Alyssa postulates the existence of an abstract object called an “interval” that has two endpoints: a lower bound and an upper bound.
;; She also presumes that, given the endpoints of an interval, she can construct the interval using the data constructor make-interval.
;; Alyssa’s program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:
(define (make-interval a b) (cons a b))
;; Define selectors upper-bound and lower-bound to complete the implementation.
(define (lower-bound i)
  (car i))
(define (upper-bound i)
  (cdr i))


;; Existing add/mul/div functions described in the text
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval-spanszero x y)   ;; modified name because ex2.10 improves it.
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))



;;;;;;;;;;;;;
;; Exercise 2.8
;; Using reasoning analogous to Alyssa’s, describe how the difference of two intervals may be computed.
;; Define a corresponding subtraction procedure, called sub-interval.
;;;;;;;;;;;;;
(define (sub-interval a b)
  (make-interval
     (- (lower-bound a) (upper-bound b))
     (- (upper-bound a) (lower-bound b))
 )
)


;;;;;;;;;;;;;
;; Exercise 2.9
;; The width of an interval is half of the difference between its upper and lower bounds.
;; The width is a measure of the uncertainty of the number specified by the interval.
;; For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals,
;; whereas for others the width of the combination is not a function of the widths of the argument intervals.
;; Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted).
;; Give examples to show that this is not true for multiplication or division.
;;;;;;;;;;;;;
(define (interval-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)
)
;; unit tests show addition and subtraction of two intervals always adds the widths together
;; unit tests show that division and multiplication depend on not just the width, but also the actual numerical values of start points and end points.


;;;;;;;;;;;;
;; Exercise 2.10
;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and comments that
;   it is not clear what it means to divide by an interval that spans zero.
;; Modify Alyssa’s code to check for this condition and to signal an error if it occurs.
(define (div-interval x y)
    (define (spans-zero? i)
	(and
	  (not (> (lower-bound i) 0))
          (not (< (upper-bound i) 0))))
    (if (spans-zero? y)
	(error "The dividing interval cannot span 0.")
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y))))))
