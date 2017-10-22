#lang info
(define collection "gtp-checkup")
(define deps '(
  "base"
  "data-lib"
  "math-lib"
  "memoize"
  "pict-lib"
  "plot-lib"
  "rackunit-lib"
  "sandbox-lib"
  "typed-racket-lib"
  "typed-racket-more"
  "zo-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Gradual typing correctness check")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/gtp-checkup.scrbl" ())))
