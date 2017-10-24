#lang typed/racket/base

(require
         "typed-data.rkt")

(require/typed "sequencer.rkt"
  [note (-> Symbol Natural Natural (Pairof Natural Natural))]
  [sequence (-> Natural (Listof (Pairof (U Natural #f) Natural)) Natural (-> Float (-> Indexes Float)) Array)])

(require/typed "drum.rkt"
  [drum (-> Natural Pattern Natural Array)])

(require/typed "mixer.rkt"
  [mix (-> Weighted-Signal * Array)])

(require/typed "synth.rkt"
  [emit (-> Array (Vectorof Integer))]
  [sawtooth-wave (-> Float (-> Indexes Float))])

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
(: small-test (-> (Vectorof Integer)))
(define (small-test)
  (emit
   (mix/sugar
    (sequence 1 (list
      (note 'C 5 1)
      (cons #f 1)
      (note 'C 5 1))
      1200 sawtooth-wave)
    (drum 1 '(O #f #f #f X) 1200))))

(: main (-> Void))
(define (main)
  (small-test)
  (void))

(time (main))
