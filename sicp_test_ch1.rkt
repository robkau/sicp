#lang sicp

(#%require rackunit "sicp.rkt")


(define (run-tests)
  (test-case
   "Sample test"
   (check-equal? (sum-of-squares 2 3) 13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
   "simple primes"
   (check-equal? (prime? 3) #t)
   (check-equal? (prime? 4) #f)
   (check-equal? (prime? 982446271) #t)
   (check-equal? (prime? 982446273) #f)
   (check-equal? (prime? 982446313) #t)
   (check-equal? (prime? 982446315) #f)
   (check-equal? (prime? 982446323) #t)
   (check-equal? (prime? 982446325) #f)

   (check-equal? (fast-prime? 3 3) #t)
   (check-equal? (fast-prime? 4 10) #f)
   (check-equal? (fast-prime? 561 30) #t)  ;; carmichael number false positive!
   (check-equal? (fast-prime? 982446271 3) #t)
   (check-equal? (fast-prime? 982446273 3) #f)
   (check-equal? (fast-prime? 982446313 3) #t)
   (check-equal? (fast-prime? 982446315 3) #f)
   (check-equal? (fast-prime? 982446323 3) #t)
   (check-equal? (fast-prime? 982446325 3) #f)



   (map (lambda (x) (check-equal? (carmichael-num? x) #t (number->string x)))
        '(561 1105 1729 2465 2821 6601 8911 10585 15841 29341 41041 46657 52633 62745))

   (map (lambda (x) (check-equal? (carmichael-num? x) #f (number->string x )))
        '(562 1104 1733 2469 2822 6603 8922 10581 15844 29342 41222 46333 52423))

   (map (lambda (x) (check-equal? (miller-rabin-test x 40) #t)) '(2 3 5 7 11 13 17))

   ;;(check-equal? (miller-rabin-test 0 40) #f)
   ;;(check-equal? (miller-rabin-test 1 40) #f)
   (check-equal? (miller-rabin-test 4 40) #f)
   (check-equal? (miller-rabin-test 561 40) #f)  ;; carmichael numbers handled correctly
   (check-equal? (miller-rabin-test 1105 40) #f)
   (check-equal? (miller-rabin-test 1729 40) #f)
   (check-equal? (miller-rabin-test 2465 40) #f)
   (check-equal? (miller-rabin-test 2821 40) #f)
   (check-equal? (miller-rabin-test 6601 40) #f)
   (check-equal? (miller-rabin-test 8911 40) #f)
   (check-equal? (miller-rabin-test 10585 40) #f)
   (check-equal? (miller-rabin-test 15841 40) #f)
   (check-equal? (miller-rabin-test 29341 40) #f)
   (check-equal? (miller-rabin-test 41041 40) #f)
   (check-equal? (miller-rabin-test 46657 40) #f)
   (check-equal? (miller-rabin-test 52633 40) #f)
   (check-equal? (miller-rabin-test 62745 40) #f)



   ;; big numbers like to be a false negative, needs a lot of iterations
   ;; to improve can pick the more likely and better witnesses rather than totally random.
   ;; can also review that stackoverflow post talking about the witness being wrong in the text.
   (check-equal? (miller-rabin-test 982446271 40) #t)
   ;(check-equal? (miller-rabin-test 982446273 500000) #f)
   (check-equal? (miller-rabin-test 982446313 40) #t)
   ;(check-equal? (miller-rabin-test 982446315 500000) #f)
   (check-equal? (miller-rabin-test 982446323 40) #t)
   ;(check-equal? (miller-rabin-test 982446325 500000) #f)


   (check-equal? (miller-rabin-test 1198927 40) #t)
   (check-equal? (miller-rabin-test 1198949 40) #t)
   (check-equal? (miller-rabin-test 1198973 40) #t)
   (check-equal? (miller-rabin-test 1198979 40) #t)
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
   "series"


(check-equal? (sum cube 1 inc 2) (sum-recursive cube 1 inc 2))
(check-equal? (sum cube 1 inc 200) (sum-recursive cube 1 inc 200))
(check-within (pi-sum 1 40 sum-recursive) (pi-sum 1 40 sum) 1e-10)


   (check-equal? (sum-cubes 1 10) 3025)
   (check-equal? (sum-integers 1 10) 55)
   (check-equal? (* 8 (pi-sum 1 1000 sum-recursive)) 3.139592655589783)
   (check-equal? (integral cube 0 1 0.01) 0.24998750000000042)
   (check-within (integral cube 0 1 0.001) 0.249999875000001 1e-10)

   (check-within (simpson-integral cube 0 1 2) 0.25 1e-10)
   (check-within (simpson-integral cube 0 1 100) 0.25 1e-10)
   (check-within (simpson-integral cube 0 1 1000) 0.25 1e-10)

   ;; https://www.wolframalpha.com/input?i=integrate+x%5E7-44x%5E5%2B0.17x%5E2%2B1%2F17%28x%29+from+0+to+2
   (check-within (simpson-integral some-polynomial 0 2 1000) -436.76235294181674 1e-10) ;; todo this does not get more accurate with more iterations
   (check-within (integral some-polynomial 0 2 0.0001)       -436.76235166124667 1e-10)
  )
)


(run-tests)
(display "tests done")
