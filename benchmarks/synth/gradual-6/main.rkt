#lang racket/base

(require (only-in "sequencer.rkt" note sequence)
         (only-in "drum.rkt" drum)
         (only-in "mixer.rkt" mix)
         (only-in "synth.rkt" emit sawtooth-wave))

(require (for-syntax racket/base syntax/parse) racket/stxparam)

(begin-for-syntax
 (define-syntax-class mixand
   #:attributes (signal weight)
   (pattern [signal:expr (~datum #:weight) weight:expr])
   (pattern signal:expr #:with weight #'1)))

(define-syntax (mix/sugar stx)
  (syntax-parse stx
    [(_ sig:mixand ...)
     #'(mix (list sig.signal sig.weight) ...)]))

;; Small test, for development
(define (small-test)
  (emit
   (mix/sugar
    (sequence 1 (list
      (note 'C 5 1)
      (cons #f 1)
      (note 'C 5 1))
      1200 sawtooth-wave)
    (drum 1 '(O #f #f #f X) 1200))))

(define (main)
  (small-test)
  (void))

(time (main))
