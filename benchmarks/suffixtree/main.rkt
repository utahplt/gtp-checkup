#lang typed/racket/base

(require
 (only-in racket/file file->lines file->string))

(require/typed "lcs.rkt"
  [longest-common-substring (-> String String String)])

(define SMALL_TEST "test.txt")

;; LCS on all pairs of lines in a file
(: main (-> String Void))
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(time (main SMALL_TEST))
