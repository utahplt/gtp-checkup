#lang racket/base

(require racket/contract)
(provide
  flat-hash/c
  (contract-out
    (configuration-name*
      (listof symbol?))
    (benchmark-name?
      (-> any/c boolean?))
    (cpu-time?
      (-> any/c boolean?))
    (cpu-time*?
      (-> any/c boolean?))
    (timeout?
      (-> any/c boolean?))
    (timeout->time-limit
      (-> timeout? exact-nonnegative-integer?))
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
    (make-commit-data
      (-> commit-name? benchmarks-data? commit-data?))
    (struct machine-data
            ((id machine-name?)
             (commit* (listof commit-data?))))
    (make-machine-data
      (-> machine-name? (listof commit-data?) machine-data?))))

;; -----------------------------------------------------------------------------

(define (flat-hash/c k v)
  (hash/c k v #:immutable #true #:flat? #true))

(define benchmark-name? symbol?)
;; additionally matches the name of a `benchmarks/` folder

(define cpu-time? exact-nonnegative-integer?)

(define cpu-time*? (listof cpu-time?))

(define timeout? (cons/c 'timeout exact-nonnegative-integer?))

(define timeout->time-limit cdr)

(define configuration-data? (or/c 'error cpu-time*? timeout?))

(define configuration-name? (or/c 'untyped 'typed 'typed-worst-case))

(define configuration-name* '(untyped typed typed-worst-case))

(define benchmarks-data?
  (flat-hash/c benchmark-name?
          (flat-hash/c configuration-name?  configuration-data?)))

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

