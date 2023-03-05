#lang typed/racket/base #:no-optimize

(require "card-adapted.rkt")
(provide Stack)
(define-type Stack
  (Listof Card))
