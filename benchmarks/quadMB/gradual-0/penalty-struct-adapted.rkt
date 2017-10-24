#lang typed/racket/base

(provide (struct-out $penalty))

;; -----------------------------------------------------------------------------

(require
  "../base/core-types.rkt")

(require/typed "penalty-struct.rkt"
  [#:struct $penalty ([hyphens : Nonnegative-Integer]
                      [width : Value-Type])])

;; =============================================================================

(define-type Value-Type Float)
