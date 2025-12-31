#lang sicp

(#%require "sicp.rkt")

;; Simple test framework for SICP
(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (assert-equal actual expected message)
  (set! test-count (+ test-count 1))
  (if (= actual expected)
      (begin
        (set! pass-count (+ pass-count 1))
        (display "  ✓ ")
        (display message)
        (newline))
      (begin
        (set! fail-count (+ fail-count 1))
        (display "  ✗ ")
        (display message)
        (display " - Expected: ")
        (display expected)
        (display ", Got: ")
        (display actual)
        (newline))))

(define (test-case name . body)
  (display name)
  (newline)
  (apply (lambda () body) '())
  (newline))

(define (run-tests)
  (display "Running SICP Tests...")
  (newline)
  (display "=====================")
  (newline)
  (newline))

(define (summary)
  (newline)
  (display "=====================")
  (newline)
  (display "Tests run: ")
  (display test-count)
  (newline)
  (display "Passed: ")
  (display pass-count)
  (newline)
  (display "Failed: ")
  (display fail-count)
  (newline))

;; Chapter 2: Rational Numbers Tests

;; Test GCD with negative numbers
(test-case "gcd should handle negative numbers"
  (assert-equal (gcd 8 4) 4 "gcd of two positives")
  (assert-equal (gcd -8 4) 4 "gcd with negative first argument")
  (assert-equal (gcd 8 -4) 4 "gcd with negative second argument")
  (assert-equal (gcd -8 -4) 4 "gcd with both negative")
  (assert-equal (gcd 4 8) 4 "flipped gcd of two positives")
  (assert-equal (gcd -4 8) 4 "flipped gcd with negative first argument")
  (assert-equal (gcd 4 -8) 4 "flipped gcd with negative second argument")
  (assert-equal (gcd -4 -8) 4 "flipped gcd with both negative"))

(test-case "one-half should be 1/2"
  (assert-equal (numer (one-half)) 1 "numerator should be 1")
  (assert-equal (denom (one-half)) 2 "denominator should be 2"))

(test-case "one-third should be 1/3"
  (assert-equal (numer (one-third)) 1 "numerator should be 1")
  (assert-equal (denom (one-third)) 3 "denominator should be 3"))

(test-case "add-rat one-half one-third should be 5/6"
  (let ((result (add-rat (one-half) (one-third))))
    (assert-equal (numer result) 5 "numerator should be 5")
    (assert-equal (denom result) 6 "denominator should be 6")))

(test-case "mul-rat one-half one-third should be 1/6"
  (let ((result (mul-rat (one-half) (one-third))))
    (assert-equal (numer result) 1 "numerator should be 1")
    (assert-equal (denom result) 6 "denominator should be 6")))

(test-case "add-rat one-third one-third without GCD reduction should be 6/9"
  ;; Create rationals without GCD reduction
  (let ((one-third-simple (make-rat-simple 1 3)))
    (let ((result (cons (+ (* (numer one-third-simple) (denom one-third-simple))
                           (* (numer one-third-simple) (denom one-third-simple)))
                        (* (denom one-third-simple) (denom one-third-simple)))))
      (assert-equal (car result) 6 "numerator should be 6")
      (assert-equal (cdr result) 9 "denominator should be 9"))))

(test-case "add-rat one-third one-third with make-rat-gcd should be 2/3"
  (let ((one-third-gcd (make-rat-gcd 1 3)))
    (let ((result (add-rat one-third-gcd one-third-gcd)))
      (assert-equal (numer result) 2 "numerator should be 2")
      (assert-equal (denom result) 3 "denominator should be 3"))))

;; Exercise 2.1: Test make-rat with sign normalization
(test-case "make-rat with two positives should normalize"
  (let ((result (make-rat 4 8)))
    (assert-equal (numer result) 1 "numerator should be 1")
    (assert-equal (denom result) 2 "denominator should be 2")))

(test-case "make-rat with positive numerator and negative denominator"
  (let ((result (make-rat 4 -8)))
    (assert-equal (numer result) -1 "numerator should be -1")
    (assert-equal (denom result) 2 "denominator should be 2")))

(test-case "make-rat with negative numerator and positive denominator"
  (let ((result (make-rat -4 8)))
    (assert-equal (numer result) -1 "numerator should be -1")
    (assert-equal (denom result) 2 "denominator should be 2")))

(test-case "make-rat with two negatives should become positive"
  (let ((result (make-rat -4 -8)))
    (assert-equal (numer result) 1 "numerator should be 1")
    (assert-equal (denom result) 2 "denominator should be 2")))

;; Exercise 2.2: Points and Segments Tests

(test-case "make-point should create a point with x and y coordinates"
  (let ((p (make-point 3 4)))
    (assert-equal (x-point p) 3 "x coordinate should be 3")
    (assert-equal (y-point p) 4 "y coordinate should be 4")))

(test-case "make-point with negative coordinates"
  (let ((p (make-point -5 -7)))
    (assert-equal (x-point p) -5 "x coordinate should be -5")
    (assert-equal (y-point p) -7 "y coordinate should be -7")))

(test-case "make-point with zero coordinates"
  (let ((p (make-point 0 0)))
    (assert-equal (x-point p) 0 "x coordinate should be 0")
    (assert-equal (y-point p) 0 "y coordinate should be 0")))

(test-case "make-segment should create a segment with start and end points"
  (let ((p1 (make-point 1 2)))
    (let ((p2 (make-point 5 6)))
      (let ((seg (make-segment p1 p2)))
        (assert-equal (x-point (start-segment seg)) 1 "start x should be 1")
        (assert-equal (y-point (start-segment seg)) 2 "start y should be 2")
        (assert-equal (x-point (end-segment seg)) 5 "end x should be 5")
        (assert-equal (y-point (end-segment seg)) 6 "end y should be 6"))
      (let ((seg (make-segment p2 p1)))
        (assert-equal (x-point (start-segment seg)) 5 "start x should be 5")
        (assert-equal (y-point (start-segment seg)) 6 "start y should be 6")
        (assert-equal (x-point (end-segment seg)) 1 "end x should be 1")
        (assert-equal (y-point (end-segment seg)) 2 "end y should be 2")))))

(test-case "midpoint-segment with positive coordinates"
  (let ((p1 (make-point 0 0)))
    (let ((p2 (make-point 4 6)))
      (let ((seg (make-segment p1 p2)))
        (let ((mid (midpoint-segment seg)))
          (assert-equal (x-point mid) 2 "midpoint x should be 2")
          (assert-equal (y-point mid) 3 "midpoint y should be 3"))))))

(test-case "midpoint-segment with negative coordinates"
  (let ((p1 (make-point -4 -6)))
    (let ((p2 (make-point 4 6)))
      (let ((seg (make-segment p1 p2)))
        (let ((mid (midpoint-segment seg)))
          (assert-equal (x-point mid) 0 "midpoint x should be 0")
          (assert-equal (y-point mid) 0 "midpoint y should be 0"))))))

(test-case "midpoint-segment with one negative coordinate"
  (let ((p1 (make-point -2 3)))
    (let ((p2 (make-point 6 -1)))
      (let ((seg (make-segment p1 p2)))
        (let ((mid (midpoint-segment seg)))
          (assert-equal (x-point mid) 2 "midpoint x should be 2")
          (assert-equal (y-point mid) 1 "midpoint y should be 1"))))))

(test-case "midpoint-segment with zero endpoints"
  (let ((p1 (make-point 0 0)))
    (let ((p2 (make-point 0 0)))
      (let ((seg (make-segment p1 p2)))
        (let ((mid (midpoint-segment seg)))
          (assert-equal (x-point mid) 0 "midpoint x should be 0")
          (assert-equal (y-point mid) 0 "midpoint y should be 0"))))))

;; Exercise 2.3: Rectangle Tests (naive representation)

;; AABB Non-Square Tests
(test-case "area-naive of AABB non-square rectangle 3x4 at origin"
  (let ((p1 (make-point 0 0)))
    (let ((p2 (make-point 3 0)))
      (let ((p3 (make-point 3 4)))
        (let ((p4 (make-point 0 4)))
          (let ((left (make-segment p1 p4)))
            (let ((bottom (make-segment p1 p2)))
              (let ((right (make-segment p2 p3)))
                (let ((top (make-segment p4 p3)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (area-naive rect) 12 "area should be 12")))))))))))

(test-case "perimeter-naive of AABB non-square rectangle 3x4 at origin"
  (let ((p1 (make-point 0 0)))
    (let ((p2 (make-point 3 0)))
      (let ((p3 (make-point 3 4)))
        (let ((p4 (make-point 0 4)))
          (let ((left (make-segment p1 p4)))
            (let ((bottom (make-segment p1 p2)))
              (let ((right (make-segment p2 p3)))
                (let ((top (make-segment p4 p3)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (perimeter-naive rect) 14 "perimeter should be 14")))))))))))

(test-case "area-naive of AABB non-square rectangle 5x2 offset"
  (let ((p1 (make-point 1 1)))
    (let ((p2 (make-point 6 1)))
      (let ((p3 (make-point 6 3)))
        (let ((p4 (make-point 1 3)))
          (let ((left (make-segment p1 p4)))
            (let ((bottom (make-segment p1 p2)))
              (let ((right (make-segment p2 p3)))
                (let ((top (make-segment p4 p3)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (area-naive rect) 10 "area should be 10")))))))))))

(test-case "perimeter-naive of AABB non-square rectangle 5x2 offset"
  (let ((p1 (make-point 1 1)))
    (let ((p2 (make-point 6 1)))
      (let ((p3 (make-point 6 3)))
        (let ((p4 (make-point 1 3)))
          (let ((left (make-segment p1 p4)))
            (let ((bottom (make-segment p1 p2)))
              (let ((right (make-segment p2 p3)))
                (let ((top (make-segment p4 p3)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (perimeter-naive rect) 14 "perimeter should be 14")))))))))))

;; AABB Square Tests
(test-case "area-naive of AABB square 4x4"
  (let ((p1 (make-point 0 0)))
    (let ((p2 (make-point 4 0)))
      (let ((p3 (make-point 4 4)))
        (let ((p4 (make-point 0 4)))
          (let ((left (make-segment p1 p4)))
            (let ((bottom (make-segment p1 p2)))
              (let ((right (make-segment p2 p3)))
                (let ((top (make-segment p4 p3)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (area-naive rect) 16 "area should be 16")))))))))))

(test-case "perimeter-naive of AABB square 4x4"
  (let ((p1 (make-point 0 0)))
    (let ((p2 (make-point 4 0)))
      (let ((p3 (make-point 4 4)))
        (let ((p4 (make-point 0 4)))
          (let ((left (make-segment p1 p4)))
            (let ((bottom (make-segment p1 p2)))
              (let ((right (make-segment p2 p3)))
                (let ((top (make-segment p4 p3)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (perimeter-naive rect) 16 "perimeter should be 16")))))))))))

;; Rotated Square Tests
(test-case "area-naive of rotated square (diamond) with side length 2√2"
  (let ((p1 (make-point 0 2)))
    (let ((p2 (make-point 2 0)))
      (let ((p3 (make-point 4 2)))
        (let ((p4 (make-point 2 4)))
          (let ((left (make-segment p1 p2)))
            (let ((bottom (make-segment p2 p3)))
              (let ((right (make-segment p3 p4)))
                (let ((top (make-segment p4 p1)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (area-naive rect) 7.999999999999998 "area should be 8")))))))))))

(test-case "perimeter-naive of rotated square (diamond) with side length 2√2"
  (let ((p1 (make-point 0 2)))
    (let ((p2 (make-point 2 0)))
      (let ((p3 (make-point 4 2)))
        (let ((p4 (make-point 2 4)))
          (let ((left (make-segment p1 p2)))
            (let ((bottom (make-segment p2 p3)))
              (let ((right (make-segment p3 p4)))
                (let ((top (make-segment p4 p1)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (perimeter-naive rect) 11.31370849898476 "perimeter should be 8√2")))))))))))

;; Rotated Non-Square Rectangle Tests
(test-case "area-naive of rotated non-square rectangle"
  (let ((p1 (make-point 1 0)))
    (let ((p2 (make-point 5 2)))
      (let ((p3 (make-point 4 4)))
        (let ((p4 (make-point 0 2)))
          (let ((left (make-segment p1 p4)))
            (let ((bottom (make-segment p1 p2)))
              (let ((right (make-segment p2 p3)))
                (let ((top (make-segment p4 p3)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (area-naive rect) 10.000000000000002 "area should be √5 * 2√5 = 10")))))))))))

(test-case "perimeter-naive of rotated non-square rectangle"
  (let ((p1 (make-point 1 0)))
    (let ((p2 (make-point 5 2)))
      (let ((p3 (make-point 4 4)))
        (let ((p4 (make-point 0 2)))
          (let ((left (make-segment p1 p4)))
            (let ((bottom (make-segment p1 p2)))
              (let ((right (make-segment p2 p3)))
                (let ((top (make-segment p4 p3)))
                  (let ((rect (make-rect-naive left bottom right top)))
                    (assert-equal (perimeter-naive rect) 13.416407864998739 "perimeter should be 6√5")))))))))))

;; Exercise 2.3: Rectangle Tests (compact representation with origin/width/height/angle)

;; AABB Non-Square Tests
(test-case "area of AABB non-square rectangle 3x4 at origin (compact)"
  (let ((rect (make-rect (make-point 0 0) 3 4 0)))
    (assert-equal (area rect) 12 "area should be 12")))

(test-case "perimeter of AABB non-square rectangle 3x4 at origin (compact)"
  (let ((rect (make-rect (make-point 0 0) 3 4 0)))
    (assert-equal (perimeter rect) 14 "perimeter should be 14")))

(test-case "area of AABB non-square rectangle 5x2 offset (compact)"
  (let ((rect (make-rect (make-point 1 1) 5 2 0)))
    (assert-equal (area rect) 10 "area should be 10")))

(test-case "perimeter of AABB non-square rectangle 5x2 offset (compact)"
  (let ((rect (make-rect (make-point 1 1) 5 2 0)))
    (assert-equal (perimeter rect) 14 "perimeter should be 14")))

;; AABB Square Tests
(test-case "area of AABB square 4x4 (compact)"
  (let ((rect (make-rect (make-point 0 0) 4 4 0)))
    (assert-equal (area rect) 16 "area should be 16")))

(test-case "perimeter of AABB square 4x4 (compact)"
  (let ((rect (make-rect (make-point 0 0) 4 4 0)))
    (assert-equal (perimeter rect) 16 "perimeter should be 16")))

;; Rotated Square Tests
(test-case "area of rotated square with side length 2√2 (compact)"
  (let ((rect (make-rect (make-point 2 0) 2.8284271247461903 2.8284271247461903 0.7853981633974483)))
    (assert-equal (area rect) 8.000000000000002 "area should be 8")))

(test-case "perimeter of rotated square with side length 2√2 (compact)"
  (let ((rect (make-rect (make-point 2 0) 2.8284271247461903 2.8284271247461903 0.7853981633974483)))
    (assert-equal (perimeter rect) 11.313708498984761 "perimeter should be 8√2")))

;; Rotated Non-Square Rectangle Tests
(test-case "area of rotated non-square rectangle (compact)"
  (let ((rect (make-rect (make-point 1 0) 4.47213595499958 2.23606797749979 0)))
    (assert-equal (area rect) 10.000000000000002 "area should be √5 * 2√5 = 10")))

(test-case "perimeter of rotated non-square rectangle (compact)"
  (let ((rect (make-rect (make-point 1 0) 4.47213595499958 2.23606797749979 0)))
    (assert-equal (perimeter rect) 13.416407864998739 "perimeter should be 6√5")))

;; Exercise 2.4: Procedural Pair Representation Tests

(test-case "car-alt should extract first element from cons-alt pair"
  (let ((pair (cons-alt 5 10)))
    (assert-equal (car-alt pair) 5 "car-alt should return 5")))

(test-case "cdr-alt should extract second element from cons-alt pair"
  (let ((pair (cons-alt 5 10)))
    (assert-equal (cdr-alt pair) 10 "cdr-alt should return 10")))

(test-case "cons-alt with negative numbers"
  (let ((pair (cons-alt -3 -7)))
    (assert-equal (car-alt pair) -3 "car-alt should return -3")
    (assert-equal (cdr-alt pair) -7 "cdr-alt should return -7")))

(test-case "cons-alt with zero"
  (let ((pair (cons-alt 0 0)))
    (assert-equal (car-alt pair) 0 "car-alt should return 0")
    (assert-equal (cdr-alt pair) 0 "cdr-alt should return 0")))

(test-case "cons-alt with different values"
  (let ((pair (cons-alt 100 200)))
    (assert-equal (car-alt pair) 100 "car-alt should return 100")
    (assert-equal (cdr-alt pair) 200 "cdr-alt should return 200")))

(test-case "nested cons-alt pairs"
  (let ((inner-pair (cons-alt 3 4)))
    (let ((outer-pair (cons-alt 1 inner-pair)))
      (assert-equal (car-alt outer-pair) 1 "outer car-alt should return 1")
      (assert-equal (car-alt (cdr-alt outer-pair)) 3 "nested car-alt should return 3")
      (assert-equal (cdr-alt (cdr-alt outer-pair)) 4 "nested cdr-alt should return 4"))))

;; Exercise 2.5: Integer Power Pair Representation Tests
(test-case "count-divisions helper function"
  (assert-equal (count-divisions 10 2) 1 "10 evenly divides by 2 one time")
 (assert-equal (count-divisions 11 2) 0 "11 evenly divides by 2 zero times")
 (assert-equal (count-divisions 12 2) 2 "12 evenly divides by 2 two times")
 (assert-equal (count-divisions 21 3) 1 "21 evenly divides by 3 one time")
 (assert-equal (count-divisions 24 3) 1 "24 evenly divides by 3 one time")
 (assert-equal (count-divisions 27 3) 3 "27 evenly divides by 3 three times"))



(define (test-intpow-pair a b)
  (cond ((> a 10) #t)
        ((> b 10) (test-intpow-pair (+ a 1) 0))
        (else
          (let ((pair (cons-intpow a b)))
            (assert-equal (car-intpow pair) a "car-intpow should return a")
            (assert-equal (cdr-intpow pair) b "cdr-intpow should return b")
            (test-intpow-pair a (+ b 1))))))

(test-case "cons-intpow/car-intpow/cdr-intpow comprehensive test 0 to 10"
  (test-intpow-pair 0 0))

(test-case "cons-intpow with large prime-like values"
  (let ((pair (cons-intpow 127 131)))
    (assert-equal (car-intpow pair) 127 "should extract 127")
    (assert-equal (cdr-intpow pair) 131 "should extract 131")))

(test-case "cons-intpow with very large a, small b"
  (let ((pair (cons-intpow 500 1)))
    (assert-equal (car-intpow pair) 500 "should extract 500")
    (assert-equal (cdr-intpow pair) 1 "should extract 1")))

(test-case "cons-intpow with small a, very large b"
  (let ((pair (cons-intpow 1 500)))
    (assert-equal (car-intpow pair) 1 "should extract 1")
    (assert-equal (cdr-intpow pair) 500 "should extract 500")))

(test-case "cons-intpow with both zero"
  (let ((pair (cons-intpow 0 0)))
    (assert-equal (car-intpow pair) 0 "should extract 0")
    (assert-equal (cdr-intpow pair) 0 "should extract 0")))

(test-case "cons-intpow with a=0, b=large"
  (let ((pair (cons-intpow 0 250)))
    (assert-equal (car-intpow pair) 0 "should extract 0")
    (assert-equal (cdr-intpow pair) 250 "should extract 250")))

(test-case "cons-intpow with a=large, b=0"
  (let ((pair (cons-intpow 250 0)))
    (assert-equal (car-intpow pair) 250 "should extract 250")
    (assert-equal (cdr-intpow pair) 0 "should extract 0")))

(test-case "cons-intpow with Mersenne-like exponents"
  (let ((pair (cons-intpow 31 127)))
    (assert-equal (car-intpow pair) 31 "should extract 31")
    (assert-equal (cdr-intpow pair) 127 "should extract 127")))

(test-case "cons-intpow with twin prime-like values"
  (let ((pair (cons-intpow 197 199)))
    (assert-equal (car-intpow pair) 197 "should extract 197")
    (assert-equal (cdr-intpow pair) 199 "should extract 199")))

(test-case "cons-intpow with powers of 10 magnitude"
  (let ((pair (cons-intpow 1000 1000)))
    (assert-equal (car-intpow pair) 1000 "should extract 1000")
    (assert-equal (cdr-intpow pair) 1000 "should extract 1000")))

;; Exercise 2.6: Church Numerals Tests

(define (inc x) (+ x 1))

(test-case "zero-church represents 0"
  (assert-equal ((zero-church inc) 0) 0 "zero-church should apply inc 0 times"))

(test-case "one-church represents 1"
  (assert-equal ((one-church inc) 0) 1 "one-church should apply inc 1 time"))

(test-case "two-church represents 2"
  (assert-equal ((two-church inc) 0) 2 "two-church should apply inc 2 times"))

(test-case "three-church represents 3"
  (assert-equal ((three-church inc) 0) 3 "three-church should apply inc 3 times"))

(test-case "add-1-church on zero-church gives one"
  (assert-equal (((add-1-church zero-church) inc) 0) 1 "add-1-church zero-church should equal 1"))

(test-case "add-1-church on one-church gives two"
  (assert-equal (((add-1-church one-church) inc) 0) 2 "add-1-church one-church should equal 2"))

(test-case "add-1-church on two-church gives three"
  (assert-equal (((add-1-church two-church) inc) 0) 3 "add-1-church two-church should equal 3"))

(test-case "Church numerals work with different functions"
  (let ((double (lambda (x) (* x 2))))
    (assert-equal ((zero-church double) 1) 1 "zero-church double: 1")
    (assert-equal ((one-church double) 1) 2 "one-church double: 2")
    (assert-equal ((two-church double) 1) 4 "two-church double: 4")
    (assert-equal ((three-church double) 1) 8 "three-church double: 8")))

;; Exercise 2.7 & 2.8: Interval Arithmetic Tests

(test-case "make-interval creates interval with lower and upper bounds"
  (let ((i (make-interval 1 5)))
    (assert-equal (lower-bound i) 1 "lower bound should be 1")
    (assert-equal (upper-bound i) 5 "upper bound should be 5")))

(test-case "make-interval with negative values"
  (let ((i (make-interval -10 -3)))
    (assert-equal (lower-bound i) -10 "lower bound should be -10")
    (assert-equal (upper-bound i) -3 "upper bound should be -3")))

(test-case "make-interval spanning zero"
  (let ((i (make-interval -5 5)))
    (assert-equal (lower-bound i) -5 "lower bound should be -5")
    (assert-equal (upper-bound i) 5 "upper bound should be 5")))

(test-case "add-interval with positive intervals"
  (let ((i1 (make-interval 1 3)))
    (let ((i2 (make-interval 2 4)))
      (let ((result (add-interval i1 i2)))
        (assert-equal (lower-bound result) 3 "lower bound should be 3")
        (assert-equal (upper-bound result) 7 "upper bound should be 7")))))

(test-case "add-interval with negative intervals"
  (let ((i1 (make-interval -5 -2)))
    (let ((i2 (make-interval -3 -1)))
      (let ((result (add-interval i1 i2)))
        (assert-equal (lower-bound result) -8 "lower bound should be -8")
        (assert-equal (upper-bound result) -3 "upper bound should be -3")))))

(test-case "sub-interval basic case"
  (let ((i1 (make-interval 5 10)))
    (let ((i2 (make-interval 1 3)))
      (let ((result (sub-interval i1 i2)))
        (assert-equal (lower-bound result) 2 "lower bound should be 5-3=2")
        (assert-equal (upper-bound result) 9 "upper bound should be 10-1=9")))))

(test-case "sub-interval with larger range"
  (let ((i1 (make-interval 10 20)))
    (let ((i2 (make-interval 5 8)))
      (let ((result (sub-interval i1 i2)))
        (assert-equal (lower-bound result) 2 "lower bound should be 10-8=2")
        (assert-equal (upper-bound result) 15 "upper bound should be 20-5=15")))))

(test-case "sub-interval with negative values"
  (let ((i1 (make-interval -2 3)))
    (let ((i2 (make-interval -5 -1)))
      (let ((result (sub-interval i1 i2)))
        (assert-equal (lower-bound result) -1 "lower bound should be -2-(-1)=-1")
        (assert-equal (upper-bound result) 8 "upper bound should be 3-(-5)=8")))))

(test-case "sub-interval proves correct logic"
  (let ((i1 (make-interval 0 10)))
    (let ((i2 (make-interval 5 15)))
      (let ((result (sub-interval i1 i2)))
        (assert-equal (lower-bound result) -15 "lower bound should be 0-15=-15")
        (assert-equal (upper-bound result) 5 "upper bound should be 10-5=5")))))

(test-case "mul-interval with positive intervals"
  (let ((i1 (make-interval 2 3)))
    (let ((i2 (make-interval 4 5)))
      (let ((result (mul-interval i1 i2)))
        (assert-equal (lower-bound result) 8 "lower bound should be 2*4=8")
        (assert-equal (upper-bound result) 15 "upper bound should be 3*5=15")))))

(test-case "mul-interval with mixed signs"
  (let ((i1 (make-interval -2 3)))
    (let ((i2 (make-interval 1 4)))
      (let ((result (mul-interval i1 i2)))
        (assert-equal (lower-bound result) -8 "lower bound should be -2*4=-8")
        (assert-equal (upper-bound result) 12 "upper bound should be 3*4=12")))))

(test-case "div-interval basic case"
  (let ((i1 (make-interval 8 12)))
    (let ((i2 (make-interval 2 4)))
      (let ((result (div-interval i1 i2)))
        (assert-equal (lower-bound result) 2.0 "lower bound should be 8/4=2")
        (assert-equal (upper-bound result) 6.0 "upper bound should be 12/2=6")))))

;; Exercise 2.9: Interval Width Tests

(test-case "interval-width with positive interval"
  (let ((i (make-interval 2 8)))
    (assert-equal (interval-width i) 3 "width should be (8-2)/2 = 3")))

(test-case "interval-width with negative interval"
  (let ((i (make-interval -10 -4)))
    (assert-equal (interval-width i) 3 "width should be (-4-(-10))/2 = 3")))

(test-case "interval-width spanning zero"
  (let ((i (make-interval -5 5)))
    (assert-equal (interval-width i) 5 "width should be (5-(-5))/2 = 5")))

(test-case "interval-width of single point"
  (let ((i (make-interval 7 7)))
    (assert-equal (interval-width i) 0 "width should be 0")))

;; Prove width is additive for addition
(test-case "width of sum equals sum of widths"
  (let ((i1 (make-interval 1 5)))
    (let ((i2 (make-interval 2 6)))
      (let ((sum (add-interval i1 i2)))
        (let ((w1 (interval-width i1)))
          (let ((w2 (interval-width i2)))
            (let ((w-sum (interval-width sum)))
              (assert-equal w-sum (+ w1 w2) "width([1,5] + [2,6]) should equal width[1,5] + width[2,6]"))))))))

;; Prove width is additive for subtraction
(test-case "width of difference equals sum of widths"
  (let ((i1 (make-interval 10 20)))
    (let ((i2 (make-interval 3 7)))
      (let ((diff (sub-interval i1 i2)))
        (let ((w1 (interval-width i1)))
          (let ((w2 (interval-width i2)))
            (let ((w-diff (interval-width diff)))
              (assert-equal w-diff (+ w1 w2) "width([10,20] - [3,7]) should equal width[10,20] + width[3,7]"))))))))

;; Prove multiplication depends on actual values, not just widths
(test-case "multiplication width depends on values not just widths"
  (let ((i1 (make-interval 1 3)))
    (let ((i2 (make-interval 10 12)))
      (let ((prod1 (mul-interval i1 i1)))
        (let ((prod2 (mul-interval i2 i2)))
          (let ((w1 (interval-width i1)))
            (let ((w2 (interval-width i2)))
              (let ((w-prod1 (interval-width prod1)))
                (let ((w-prod2 (interval-width prod2)))
                  (assert-equal w1 w2 "both intervals have same width = 1")
                  (assert-equal w-prod1 4 "width([1,3]*[1,3]) = 4")
                  (assert-equal w-prod2 22 "width([10,12]*[10,12]) = 22"))))))))))

;; Prove division depends on actual values, not just widths
(test-case "division width depends on values not just widths"
  (let ((i1 (make-interval 2 4)))
    (let ((i2 (make-interval 10 12)))
      (let ((quot1 (div-interval i1 (make-interval 1 2))))
        (let ((quot2 (div-interval i2 (make-interval 1 2))))
          (let ((w1 (interval-width i1)))
            (let ((w2 (interval-width i2)))
              (let ((w-quot1 (interval-width quot1)))
                (let ((w-quot2 (interval-width quot2)))
                  (assert-equal w1 w2 "both intervals have same width = 1")
                  (assert-equal w-quot1 1.5 "width([2,4]/[1,2]) = 1.5")
                  (assert-equal w-quot2 3.5 "width([10,12]/[1,2]) = 3.5"))))))))))



;; Exercise 2.10: in div-interval, the dividing interval is not allowed to span 0

;; Test normal division with positive divisor
(test-case "div-interval with positive divisor [2,4]"
  (let ((i1 (make-interval 10 20)))
    (let ((i2 (make-interval 2 4)))
      (let ((result (div-interval i1 i2)))
        (assert-equal (lower-bound result) 2.5 "lower bound should be 10/4 = 2.5")
        (assert-equal (upper-bound result) 10.0 "upper bound should be 20/2 = 10.0")))))

;; Test division with positive divisor [5,10]
(test-case "div-interval with positive divisor [5,10]"
  (let ((i1 (make-interval 50 100)))
    (let ((i2 (make-interval 5 10)))
      (let ((result (div-interval i1 i2)))
        (assert-equal (lower-bound result) 5.0 "lower bound should be 50/10 = 5.0")
        (assert-equal (upper-bound result) 20.0 "upper bound should be 100/5 = 20.0")))))

;; Test division with negative divisor [-10,-5]
(test-case "div-interval with negative divisor [-10,-5]"
  (let ((i1 (make-interval 50 100)))
    (let ((i2 (make-interval -10 -5)))
      (let ((result (div-interval i1 i2)))
        (assert-equal (lower-bound result) -20.0 "lower bound should be 50/(-5) = -20.0")
        (assert-equal (upper-bound result) -5.0 "upper bound should be 100/(-10) = -5.0")))))

;; Test division with negative divisor [-4,-2]
(test-case "div-interval with negative divisor [-4,-2]"
  (let ((i1 (make-interval 20 40)))
    (let ((i2 (make-interval -4 -2)))
      (let ((result (div-interval i1 i2)))
        (assert-equal (lower-bound result) -20.0 "lower bound should be 20/(-2) = -20.0")
        (assert-equal (upper-bound result) -5.0 "upper bound should be 40/(-4) = -5.0")))))

;; The following will raise error when un-commented since divisor spanning zero is no longer allowed.
;; (div-interval (make-interval 1 5) (make-interval -1 1))   ;; spans zero
;; (div-interval (make-interval 10 20) (make-interval -5 5)) ;; spans zero
;; (div-interval (make-interval 1 5) (make-interval 0 2))    ;; includes zero
;; (div-interval (make-interval 1 5) (make-interval -2 0))   ;; includes zero



(run-tests)
(summary)
