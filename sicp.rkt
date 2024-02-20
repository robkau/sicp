#lang sicp


(define
  (sum-of-squares a b)
  (+
   (* a a)
   (* b b)))



(define
  (sum-of-two-biggest-squares a b c)
  (cond ((and (> b a) (> c a)) (sum-of-squares b c))
        ((and (> a c) (> b c) (sum-of-squares a b)))
        (else (sum-of-squares a c)
              )
        )
  )



;; Running (test 0 (p)) is:
;; infinite recursion when using applicative order evaluation
;; runs fine when using normal order evaluation
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y
      ))


;; Newton's square root calculation
(define (sqrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))


(define (good-enough? guess previous-guess)
  (< (abs (- guess previous-guess)) 0.000000001)
  )



(define (square x) (* x x))

(define (sqrt x) (sqrt-iter 1.0  0.0 x))



;; Exercise 1.6, defining if without a special form.
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-newif guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-newif (improve guess x)
                           x)))
;; this is infinite recursion because of applicative order evaluation (keeps going down and evaluating the 'new-if' arguments). the special-form if only evaluates the matching predicate branch).




;; Exercise 1.8 Newton's method for cube roots.
(define (cube-root-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
      guess
      (cube-root-iter (improve-cube guess x) guess x)))
  

(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-root x) (cube-root-iter 1.0 0.0 x))



;; Exercise 1.10 Ackermann function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))


;; Exercise 1.11
;; A function f is defined by the rule that:
;; f(n)=n if n<3
;; f(n)=f(n-1) + 2*f(n-2) + 3*f(n-3) if n≥3
;; Write a procedure that computes f by means of a recursive process.
(define (111-recursive n)
  (cond ((< n 3) n)
        (else (+ (111-recursive (- n 1)) (* 2 (111-recursive (- n 2))) (* 3 (111-recursive (- n 3)))))
        )
  )
;; Write a procedure that computes f by means of an iterative process.
;; todo!



;; Exercise 1.12
;; Write a procedure that computes elements of Pascal’s triangle by means of a recursive process.
(define (pascal row col)
  (cond ((= row 1) 1)
        ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))
              
        ))

(define (display-pascal-row n)
  (define (column-iter i)
    (display (pascal n i)) (display "  ")
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
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


;; This version using recursive iteration is o (logn) steps and o(1) space.
(define (fast-expt-iter b n)
  (define (fast-expt-iter-inner b n a)
    (cond
      ((= n 0) a)
      ((even? n) (fast-expt-iter-inner (square b) (/ n 2) a))
      (else      (fast-expt-iter-inner b (- n 1) (* b a)))))
  (fast-expt-iter-inner b n 1)
  )



;; Exercise 1.17
;; Write fast multiplication algorithm using log(n) number of steps.
;; Assume double(n) and halve(n) can be used.
(define (fast-mul a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1)))
              )
        ))

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
  (cond ((= count 0)
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square q) (square p))  ; compute p'
                   (+ (* 2 p q) (square q))   ; compute q'
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q)
                      (* a q)
                      (* a p))
                   (+ (* b p)
                      (* a q))
                   p
                   q
                   (- count 1)))))



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
    (cond ((<= count 0) b ) ;; finished enough iterations. return the computed result.
          ((not (even? count)) (fib-mem-iter   ;; need to decrement by one, standard fibonacci step.
                                (+ (* b p) (* a p) (* a q))
                                (+ (* b p) (* a q))
                                p
                                q
                                (- count 1)
                                ))
          (else (fib-mem-iter       ;; opportunity to apply squared fibonnaci transformation and cut n in half.
                 a
                 b
                 (+ (square p) (square q))  ; p' calculated
                 (+ (square q) (* 2 q p))   ; q' calculated
                 (/ count 2)
                 ))
          )
    )
  (fib-mem-iter 1 0 0 1 n)
  )



;; Simple fibonnaci with recursion
(define (fib-slow n)
  (define (fib-iter-inner a b n)
    (cond ((= n 0) a)
          (else (fib-iter-inner (+ a b) a (- n 1)))
          )
    )
  (fib-iter-inner 1 0 n)
  )



;; Easy printing helper
(define (display-all . vs)
  (for-each display vs))


;; Compare fibonnaci implementations
(define (fib-combined n)
  (display-all (fib n) " "(fib-mem n) " "(fib-slow n))
  )


;; 1.2.5 Greatest Common Divisors

;; Iterative recursive process to calculate GCD of two integers
;; GCD(a,b) = GCD(b,r)
;; r = remainder(a,b)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; Finding smallest divisor of a number
(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if
   (= n 2)
   (+ n 1)
   (+ n 2)
   )
  )

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)                 ; over maximum possible
        ((divides? test-divisor n) test-divisor)        ; found smallest
        (else (find-divisor n (next test-divisor)))     ; try next
        ))



               

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))



(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base(- exp 1) m))
                    m))
        )
  )




;; Fermats little theorem
;; Probabilistic prime determination
;; (Chance for error is less than a cosmic ray flipping a bit during calculation, says the book)
;; A false positive number is a Carmichael number  (561, 1105, 1729, 2465, ...)
; Only 255 below 100,000,000
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a ))
  (try-it (+ 1 (random (- n 1)))))



(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1 )))
        (else false))
  )


;; Exercise 1.21: Use the smallest-divisor procedure to find the result for 199, 1999, 19999
; (smallest-divisor 199) 199
; (smallest-divisor 1999) 1999
; (smallest-divisor 19999) 7


;; Exercise 1.22 Using runtime primitive to measure how logn to find primes.
;; timed-prime-test takes integer n , prints n, and if n is prime prints three asterisks and the runtime.
;; Write procedure search-for-primes to find the first 3 primes larger than n, testing odd numbers only. compare if 10,100,1000... follows sqrt(10) order of growth in runtime , matching number of calculation steps.
(define (timed-prime-test n)
  (start-prime-test n (runtime))
  )

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (and
       (report-prime n (- (runtime) start-time))
       #t      
       )
      #f
      ))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  )


(define (search-for-primes n)
  (define (search-for-primes-iter n at)
    (cond ((>= at 3))
          ((even? n) (search-for-primes-iter (+ n 1) at))
          (else
           (if
            (timed-prime-test n)
            (search-for-primes-iter (+ n 2) (+ at 1))
            (search-for-primes-iter (+ n 2) at)
            )
           )))
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
  (remainder (fast-expt base exp) m)
  )
;; Original version computed primes in the millions in milliseconds, this one hung for over 10 seconds.
;; This version computes the biggest numbers in full in memory, old version breaks it down into smaller calculations and never renders the biggest number in mem.





#|
Exercise 1.26
Why is this version of expmod replacing (square x) with (* x x) changing it from O(logn) to O(n)?
|#
(define (expmod-slow base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (* (expmod-slow base (/ exp 2) m)
             (expmod-slow base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base
             (expmod-slow base (- exp 1) m))
          m))))
;; Because both arguments to (* x x) are both evaluated on each recursive call. 


#|
Exercise 1.27
Demonstrate that the Carmichael numbers listed in Footnote 47 really do fool the Fermat test.
That is, write a procedure that takes an integer n and tests whether a^n is congruent to a modulo n for every a<n,
and try your procedure on the given Carmichael numbers.
|#
(define (carmichael-num? n)
  (define (carmichael-num-iter n b)
    (cond
      ((>= b n) #t)
      ((= (expmod b n n) b) (carmichael-num-iter n (+ b 1)))
      (else #f) 
      )
    )
  (carmichael-num-iter n 1)
  )
;(display (carmichael-num? 561)) (newline)
;(display (carmichael-num? 1105)) (newline)
;(display (carmichael-num? 1729)) (newline)
;(display (carmichael-num? 2465)) (newline)
;(display (carmichael-num? 2821)) (newline)
;(display (carmichael-num? 6601)) (newline)



#|
Exercise 1.28
Miller-Rabin test.
One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test (Miller 1976; Rabin 1980). 
This starts from an alternate form of Fermat’s Little Theorem, which states that:
 if n is a prime number and a is any positive integer less than n,
 then a raised to the (n-1)-st power is congruent to 1 modulo n.

To test the primality of a number n by the Miller-Rabin test,
we pick a random number a<n and raise a to the (n-1)-st power modulo n using the expmod procedure.

However, whenever we perform the squaring step in expmod,
we check to see if we have discovered a “nontrivial square root of 1 modulo n,”
that is, a number not equal to 1 or n-1 whose square is equal to 1 modulo n.

It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime.

It is also possible to prove that if n is an odd number that is not prime, then, for at least half the numbers a<n,
computing an-1 in this way will reveal a nontrivial square root of 1 modulo n.
(This is why the Miller-Rabin test cannot be fooled.)

Modify the expmod procedure to signal if it discovers a nontrivial square root of 1,
and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test.

Check your procedure by testing various known primes and non-primes.
Hint: One convenient way to make expmod signal is to have it return 0.
|#




