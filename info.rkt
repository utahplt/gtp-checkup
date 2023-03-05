#lang info
(define collection "gtp-checkup")
(define deps '(
  "base"
  "basedir"
  "data-lib"
  "draw-lib"
  "gregor"
  "gtp-util"
  "math-lib"
  "memoize"
  "pict-lib"
  "plot-lib"
  "rackunit-lib"
  "require-typed-check"
  "sandbox-lib"
  "typed-racket-lib"
  "typed-racket-more"
  "zo-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "typed-racket-doc" "pict-abbrevs"))
(define pkg-desc "Gradual typing performance check")
(define version "1.0")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/gtp-checkup.scrbl" ())))
(define compile-omit-paths '("benchmarks/"))
