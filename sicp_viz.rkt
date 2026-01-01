#lang sicp

;; SICP Visualization Library
;; Provides simple execution tracing and visualization for function calls
;; using only cons/car/cdr/list structures

;; Import timing function from racket
(#%require (only racket/base current-milliseconds))

;; Export public API
(#%provide run print-tree)

;; ============================================================================
;; Data Structure: call-node
;; ============================================================================
;; A call-node is a list:
;; (function-name call-count total-time max-time max-time-args children)
;; where:
;;   function-name  : symbol
;;   call-count     : number (how many times this function was called)
;;   total-time     : number (sum of all execution times in milliseconds)
;;   max-time       : number (worst/longest execution time in milliseconds)
;;   max-time-args  : list (arguments that caused max-time)
;;   children       : list of call-nodes (functions called by this one)

;; Constructor
(define (make-call-node name count total-time max-time max-args children)
  (list name count total-time max-time max-args children))

;; Selectors
(define (node-name node) (car node))
(define (node-count node) (car (cdr node)))
(define (node-total-time node) (car (cdr (cdr node))))
(define (node-max-time node) (car (cdr (cdr (cdr node)))))
(define (node-max-args node) (car (cdr (cdr (cdr (cdr node))))))
(define (node-children node) (car (cdr (cdr (cdr (cdr (cdr node)))))))

;; ============================================================================
;; Tracing Infrastructure
;; ============================================================================

;; Global call stack: list of nodes currently being executed
;; Stack structure: (current-node parent-node grandparent-node ...)
;; The top of the stack is the currently executing function
;; When a function finishes, we pop it and add it to the new top's children
(define *call-stack* '())

;; Helper functions for stack manipulation (needed for macro hygiene)
(define (reset-call-stack!) (set! *call-stack* '()))
(define (get-call-stack) *call-stack*)
(define (set-call-stack! val) (set! *call-stack* val))

;; traced-call: symbol list (-> result) -> result
;; Wraps a function call with timing and tree-building logic
;;
;; STACK LOGIC:
;; 1. BEFORE call: Create node, PUSH onto stack (now "current")
;; 2. DURING call: Any nested calls will see this as "parent"
;; 3. AFTER call:  POP from stack, add to parent's children (if parent exists)
(define (traced-call name args thunk)
  (let ((start-time (current-milliseconds))
        ;; Create initial node with empty children list
        (node (make-call-node name 0 0 0 '() '())))

    ;; PUSH: Add this node to the stack (it's now "current")
    (set-call-stack! (cons node (get-call-stack)))

    ;; Execute the actual function
    (let ((result (thunk)))
      (let* ((elapsed (- (current-milliseconds) start-time))
             ;; Get the finished node from stack top (with any children added during execution)
             (finished-node (car (get-call-stack)))
             ;; Create final node with complete timing info
             (final-node (make-call-node name
                                        1
                                        elapsed
                                        elapsed
                                        args
                                        (reverse (node-children finished-node)))))

        ;; POP: Remove this node from stack
        (set-call-stack! (cdr (get-call-stack)))

        ;; If stack not empty, the new top is our PARENT
        ;; Add ourselves to parent's children list
        (if (not (null? (get-call-stack)))
            (let ((parent (car (get-call-stack))))
              ;; Modify parent's children list to include us
              (set-car! (cdr (cdr (cdr (cdr (cdr parent)))))
                       (cons final-node (node-children parent)))))

        ;; Return the result for nested calls, or the tree for top-level
        (if (null? (get-call-stack))
            final-node  ;; Stack empty = top-level call, return the tree
            result))))) ;; Stack not empty = nested call, return actual result

;; ============================================================================
;; Top-level API
;; ============================================================================

;; make-traced: symbol function -> traced-function
;; Wraps a function to add tracing on every call
(define (make-traced name original-func)
  (lambda args
    (traced-call name args (lambda () (apply original-func args)))))

;; run: MACRO that instruments a function call
;; Usage: (run func-name arg1 arg2 ...)
;; Automatically traces func-name and all its recursive calls
(define-syntax run
  (syntax-rules ()
    ((_ func-name args ...)
     (let ((original func-name)
           (traced (make-traced 'func-name func-name)))
       (reset-call-stack!)
       ;; Temporarily rebind func-name to traced version
       (set! func-name traced)
       (let ((result (func-name args ...)))
         ;; Restore original
         (set! func-name original)
         result)))))

;; ============================================================================
;; Visualization
;; ============================================================================

;; print-tree: call-node -> void
;; Prints the execution tree in text form with indentation
(define (print-tree node)
  (define (print-node node depth)
    (define indent (make-string (* depth 2) #\space))
    (display indent)
    (display (node-name node))
    (display " | calls: ")
    (display (node-count node))
    (display " | total: ")
    (display (node-total-time node))
    (display "ms | max: ")
    (display (node-max-time node))
    (display "ms | max-args: ")
    (display (node-max-args node))
    (newline)
    (for-each (lambda (child) (print-node child (+ depth 1)))
              (node-children node)))
  (print-node node 0))

;; ============================================================================
;; Helpers
;; ============================================================================

;; make-string: number char -> string
;; Creates a string of n copies of char
(define (make-string n char)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (cons char result))))
  (list->string (iter n '())))

;; ============================================================================
;; Example usage (for testing)
;; ============================================================================

;; Create a sample tree manually for testing
(define sample-tree
  (make-call-node 'fib 1 100 100 '(5)
    (list
      (make-call-node 'fib 1 60 60 '(4)
        (list
          (make-call-node 'fib 1 35 35 '(3)
            (list
              (make-call-node 'fib 1 20 20 '(2) '())
              (make-call-node 'fib 1 10 10 '(1) '())))
          (make-call-node 'fib 1 15 15 '(2) '())))
      (make-call-node 'fib 1 25 25 '(3)
        (list
          (make-call-node 'fib 1 12 12 '(2) '())
          (make-call-node 'fib 1 8 8 '(1) '()))))))

;; Uncomment to test:
;; (print-tree sample-tree)
