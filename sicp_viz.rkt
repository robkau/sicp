#lang sicp

;; SICP Visualization Library
;; Trace function execution and visualize call trees using pure list structures

(#%require (only racket/base current-milliseconds))
(#%provide run print-tree)

;; ============================================================================
;; Data Structure: call-node
;; ============================================================================
;; A call-node is a list:
;; (function-name call-count total-time max-time max-time-args children)
;; where:
;;   function-name  : symbol
;;   call-count     : number
;;   total-time     : number (milliseconds)
;;   max-time       : number (milliseconds)
;;   max-time-args  : list (arguments that caused max-time)
;;   children       : list of call-nodes

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

;; Stack of nodes being built: (current parent grandparent ...)
(define *call-stack* '())

;; Counts nested run calls (enables self-composition)
(define *run-depth* 0)

(define (reset-call-stack!) (set! *call-stack* '()))
(define (get-call-stack) *call-stack*)
(define (set-call-stack! val) (set! *call-stack* val))
(define (get-run-depth) *run-depth*)
(define (inc-run-depth!) (set! *run-depth* (+ *run-depth* 1)))
(define (dec-run-depth!) (set! *run-depth* (- *run-depth* 1)))

;; Core tracing: PUSH onto stack, execute, POP and attach to parent
(define (traced-call-impl name args thunk)
  ;; Record start time before doing anything
  (let ((start-time (current-milliseconds))
        ;; Create empty node - we'll fill in timing and children later
        (node (make-call-node name 0 0 0 '() '())))

    ;; PUSH: Add this node to front of stack - it's now "current"
    (set-call-stack! (cons node (get-call-stack)))

    ;; Execute the actual function (any nested calls will see us as parent)
    (let ((result (thunk)))

      ;; Calculate how long the function took
      (let* ((elapsed (- (current-milliseconds) start-time))
             ;; Get our node back from stack (may have children added during execution)
             (finished-node (car (get-call-stack)))
             ;; Build final node with complete timing info and reversed children
             (final-node (make-call-node name 1 elapsed elapsed args
                                        (reverse (node-children finished-node)))))

        ;; POP: Remove ourselves from stack
        (set-call-stack! (cdr (get-call-stack)))

        ;; If stack not empty, there's a parent waiting - add ourselves as its child
        (if (not (null? (get-call-stack)))
            (let ((parent (car (get-call-stack))))
              ;; Mutate parent's children list to include us
              (set-car! (cdr (cdr (cdr (cdr (cdr parent)))))
                       (cons final-node (node-children parent)))))

        ;; If stack is empty, we're top-level - return the tree
        ;; Otherwise we're nested - return the actual function result
        (if (null? (get-call-stack))
            final-node
            result)))))

;; Self-composable: traces itself only when run-depth > 1
(define (traced-call name args thunk)
  (if (> (get-run-depth) 1)
      (traced-call-impl 'traced-call (list name args 'thunk)
                       (lambda () (traced-call-impl name args thunk)))
      (traced-call-impl name args thunk)))

;; ============================================================================
;; Public API
;; ============================================================================

(define (make-traced name original-func)
  (lambda args
    (traced-call name args (lambda () (apply original-func args)))))

;; run: macro that temporarily rebinds function to traced version
;; Nested runs merge into same tree via shared stack
(define-syntax run
  (syntax-rules ()
    ((_ func-name args ...)
     (let ((is-top-level (null? (get-call-stack)))  ;; Are we starting fresh or nested?
           (original func-name)                      ;; Save original function
           (traced (make-traced 'func-name func-name))) ;; Wrap it with tracing

       ;; Increment run depth (so nested runs know they're nested)
       (inc-run-depth!)

       ;; Only reset stack if this is the top-level run (not nested)
       (if is-top-level (reset-call-stack!))

       ;; Temporarily replace the function with traced version
       (set! func-name traced)

       ;; Execute the function with given args
       (let ((result (func-name args ...)))

         ;; Restore the original function
         (set! func-name original)

         ;; Decrement run depth (we're exiting this run layer)
         (dec-run-depth!)

         ;; Return the result (tree if top-level, value if nested)
         result)))))

;; ============================================================================
;; Visualization
;; ============================================================================

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

(define (make-string n char)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (cons char result))))
  (list->string (iter n '())))
