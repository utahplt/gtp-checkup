#lang typed/racket/base #:no-optimize

;; Adapter for gregor's `structs.rkt` file.

(provide
  (struct-out YMD)
  (struct-out HMSN)
  Month)

(require
  "../base/types.rkt")

(require/typed "core-structs.rkt"
  [#:struct YMD ([y : Natural]
                 [m : Month]
                 [d : Natural])]
  [#:struct HMSN ([h : Integer]
                 [m : Integer]
                 [s : Integer]
                 [n : Integer])])

