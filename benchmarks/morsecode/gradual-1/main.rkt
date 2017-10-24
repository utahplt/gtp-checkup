#lang racket/base

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; -----------------------------------------------------------------------------

(require
  require-typed-check
  (only-in racket/file file->value))

(require (only-in "morse-code-strings.rkt"
  string->morse))

(require (only-in "levenshtein.rkt"
               string-levenshtein))

(define word-frequency-list-small "../base/frequency-small.rktd")

(define (file->words filename)
  (define words+freqs (file->value (string->path filename)))
  (for/list ([word+freq  words+freqs])
    (car word+freq)))

(define words-small (file->words word-frequency-list-small))

(define (main words)
  (for* ([w1 (in-list words)]
         [w2 (in-list words)])
    (string->morse w1)
    (string->morse w2)
    (string-levenshtein w1 w2)
    (string-levenshtein w2 w1)
    (void)))

(time (main words-small)) ;; 200ms
