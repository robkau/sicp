#lang sicp

;; Test file for sicp_viz tracing library

(#%require "sicp_viz.rkt")

;; ============================================================================
;; Define functions NORMALLY - no special syntax needed!
;; ============================================================================

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; ============================================================================
;; Test 1: Just call (run fib 4) - that's it!
;; ============================================================================

(display "Test 1: (run fib 4)")
(newline)
(newline)

(define tree (run fib 4))
(print-tree tree)

(newline)
(display "Actual fib(4) result: ")
(display (fib 4))
(newline)
(newline)

;; ============================================================================
;; Test 2: Trace a function that calls another function
;; ============================================================================

(define (sum-fibs n)
  (if (= n 0)
      0
      (+ (fib n) (sum-fibs (- n 1)))))

(display "Test 2: (run sum-fibs 3) - traces sum-fibs calling fib")
(newline)
(newline)

(define tree2 (run sum-fibs 32))
(print-tree tree2)

(newline)

;; ============================================================================
;; Test 3: Self-composition - trace the tracing itself!
;; ============================================================================

(display "Test 3: Self-composition - (run traced-fib-wrapper)")
(newline)
(display "This traces the overhead of the tracing infrastructure itself!")
(newline)
(newline)

;; Define a function whose job is to run fib
(define (traced-fib-wrapper)
  (run fib 3))

;; Now trace THAT function - shows both layers!
(define tree3 (run traced-fib-wrapper))
(print-tree tree3)

(newline)
