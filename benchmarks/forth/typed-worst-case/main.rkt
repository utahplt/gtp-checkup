#lang typed/racket/base

(require
   
  "../base/command-types.rkt")
(require/typed "eval.rkt"
  (forth-eval* (-> (Listof String) (Values Any Any)))
)
(require (only-in racket/file file->lines))

;; =============================================================================

(define LOOPS 10)

(: main (-> (Listof String) Void))
(define (main lines)
  (for ((i (in-range LOOPS)))
    (define-values [_e _s] (forth-eval* lines))
    (void)))

(define lines (file->lines "../base/history-100.txt"))

(time (main lines))
