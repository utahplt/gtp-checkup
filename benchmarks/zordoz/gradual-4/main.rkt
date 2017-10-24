#lang typed/racket/base

(require/typed "zo-shell.rkt"
  [init (-> (Vectorof String) Void)])

(define SMALL-TEST "../base/test.zo")
(define (small-test)
  (init (vector SMALL-TEST "branch")))

;; -----------------------------------------------------------------------------

(define-syntax-rule (main test)
  (with-output-to-file "/dev/null" test #:exists 'append))

(time (main small-test))
