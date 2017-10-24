#lang racket/base

(require require-typed-check
         (only-in racket/file file->lines file->string))

(require "lcs.rkt")

(define SMALL_TEST "../base/test.txt")

;; LCS on all pairs of lines in a file
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(time (main SMALL_TEST))
