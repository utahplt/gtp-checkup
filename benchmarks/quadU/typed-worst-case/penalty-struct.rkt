#lang typed/racket/base #:no-optimize

;; For wrap.rkt

(provide (struct-out $penalty))

;; =============================================================================

(struct $penalty
  ([hyphens : Natural]
   [width   : Float]
) #:transparent)

