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

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Fermats little theorem
;; Probabilistic prime determination
;; (Chance for error is less than a cosmic ray flipping a bit during calculation, says the book)
;; A false positive number is a Carmichael number  (561, 1105, 1729, 2465, ...)
; Only 255 below 100,000,000







ß∫








