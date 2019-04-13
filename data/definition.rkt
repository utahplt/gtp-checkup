#lang racket/base

(require racket/contract)
(provide
  benchmark-name?
  cpu-time?
  configuration-data?
  configuration-name?
  benchmarks-data?
  commit-name?
  commit-data?
  machine-data?)

;; -----------------------------------------------------------------------------

(define benchmark-name? symbol?)
;; additionally matches the name of a `benchmarks/` folder

(define cpu-time? exact-nonnegative-integer?)

(define configuration-data? (or/c 'error (listof cpu-time?) (cons/c 'timeout exact-nonnegative-integer?)))

(define configuration-name? (or/c 'untyped 'typed 'typed-worst-case))

(define benchmarks-data? (hash/c benchmark-name? (hash/c configuration-name? configuration-data?)))

(define commit-name?
  ;; <TIMESTAMP>_<COMMIT-HASH>.txt
  ;; e.g. 2017-01-24T12:30:46Z-0600_9078bc9efb081231f80dce6ab1939d8ba3cf112f
  (let ((rx #px"^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z[+-][0-9][0-9][0-9][0-9]_[a-z0-9]{40}$"))
    (lambda (str)
      (and (string? str) (regexp-match? rx str)))))

(define commit-data? (cons/c commit-name? benchmarks-data?))

(define machine-data? (cons/c directory-exists? (listof commit-data?)))

