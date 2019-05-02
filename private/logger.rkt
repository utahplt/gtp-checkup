#lang racket/base

(provide
  gtp-checkup-logger
  log-gtp-checkup-fatal
  log-gtp-checkup-error
  log-gtp-checkup-warning
  log-gtp-checkup-info
  log-gtp-checkup-debug)

;; -----------------------------------------------------------------------------

(define-logger gtp-checkup)
