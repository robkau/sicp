#lang sicp

(#%require "sicp_viz.rkt")

;; ============================================================================
;; Test Functions
;; ============================================================================

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (sum-fibs n)
  (if (= n 0)
      0
      (+ (fib n) (sum-fibs (- n 1)))))

;; ============================================================================
;; Test 1: Basic tracing of recursive function
;; ============================================================================
;; Shows how a simple recursive function forms a call tree
;; Each fib call appears as a node with its recursive children

(display "Test 1: (run fib 4)")
(newline)
(newline)

(define tree1 (run fib 4))
(print-tree tree1)

(newline)
(newline)

;; ============================================================================
;; Test 2: Function calling other functions
;; ============================================================================
;; Shows composition - sum-fibs calls fib multiple times
;; The traced fib calls appear as children of sum-fibs calls

(display "Test 2: (run sum-fibs 3)")
(newline)
(newline)

(define tree2 (run sum-fibs 3))
(print-tree tree2)

(newline)
(newline)

;; ============================================================================
;; Test 3: Self-composition - tracing the tracer
;; ============================================================================
;; When run-depth > 1, traced-call shows itself in the tree
;; This makes the tracing machinery itself visible
;; Outer run traces run-fib-wrapper, inner run traces fib
;; Both merge into the same call tree

(display "Test 3: Nested run calls")
(newline)
(newline)

(define (run-fib-wrapper)
  (run fib 2))

(define tree3 (run run-fib-wrapper))
(print-tree tree3)

(newline)
(newline)

;; ============================================================================
;; Test 4: Four levels of nested run calls
;; ============================================================================
;; Each wrapper adds another layer of run nesting
;; All traced-call machinery becomes visible at each level
;; Final tree shows the complete stack of instrumentation

(display "Test 4: Four nested run calls - run(run(run(run(fib(4)))))")
(newline)
(newline)

(define (wrapper-1) (run fib 4))
(define (wrapper-2) (run wrapper-1))
(define (wrapper-3) (run wrapper-2))

(define tree4 (run wrapper-3))
(print-tree tree4)

(newline)
