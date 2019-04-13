#lang racket/base

(require racket/contract)
(provide
  make-commit-data
  make-machine-data
  (contract-out
    (configuration-name*
      (listof symbol?))
    (benchmark-name?
      (-> any/c boolean?))
    (cpu-time?
      (-> any/c boolean?))
    (configuration-data?
      (-> any/c boolean?))
    (configuration-name?
      (-> any/c boolean?))
    (benchmarks-data?
      (-> any/c boolean?))
    (commit-name?
      (-> any/c boolean?))
    (struct commit-data
            ((id commit-name?)
             (benchmark# benchmarks-data?)))
    (struct machine-data
            ((id machine-name?)
             (commit* (listof commit-data?))))))

;; -----------------------------------------------------------------------------

(define benchmark-name? symbol?)
;; additionally matches the name of a `benchmarks/` folder

(define cpu-time? exact-nonnegative-integer?)

(define configuration-data? (or/c 'error (listof cpu-time?) (cons/c 'timeout exact-nonnegative-integer?)))

(define configuration-name? (or/c 'untyped 'typed 'typed-worst-case))

(define configuration-name* '(untyped typed typed-worst-case))

(define benchmarks-data?
  (hash/c benchmark-name?
          (hash/c configuration-name?
                  configuration-data?
                  #:immutable #true #:flat? #true)
          #:immutable #true #:flat? #true))

(define commit-name?
  ;; <TIMESTAMP>_<COMMIT-HASH>.txt
  ;; e.g. 2017-01-24T12:30:46Z-0600_9078bc9efb081231f80dce6ab1939d8ba3cf112f
  (let ((rx #px"^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z[+-][0-9][0-9][0-9][0-9]_[a-z0-9]{40}$"))
    (lambda (str)
      (and (string? str) (regexp-match? rx str)))))

(struct commit-data [id benchmark#] #:prefab)
(define make-commit-data commit-data)

(define machine-name? string?)
(struct machine-data [id commit*] #:prefab)
(define make-machine-data machine-data)

