#lang racket/base

(require racket/contract)
(provide
  (contract-out
    (gtp-checkup (-> racket-bin-dir/c void?))))

(require
  file/glob
  racket/path
  racket/system
  racket/runtime-path)

;; =============================================================================

(define-logger gtp-checkup)

(define-runtime-path PWD ".")

(define (contains-racket? d)
  (file-exists? (build-path d "racket")))

(define (contains-raco? d)
  (file-exists? (build-path d "raco")))

(define racket-bin-dir/c
  (and/c directory-exists?
         contains-racket?
         contains-raco?))

;; -----------------------------------------------------------------------------

(define (gtp-checkup bin-dir)
  (parameterize ([current-directory PWD])
    (for ((main (in-glob "**/main.rkt")))
      (checkup-file bin-dir main))))

(define (checkup-file bin-dir main.rkt)
  (define-values [dir name _] (split-path main.rkt))
  (define rel-dir (find-relative-path (current-directory) dir))
  (log-gtp-checkup-info "Checking '~a'" rel-dir)
  (and (raco-make bin-dir name)
       (run-racket bin-dir name)
       (void)))

(define (raco-make bin name)
  (system* (build-path bin "raco") "make" name))

(define (run-racket bin name)
  (system* (build-path bin "racket") name))

;; =============================================================================

(module* main racket/base
  (require racket/cmdline (submod ".."))
  (command-line
   #:program "gtp-checkup"
   #:args (BIN-DIR)
   (gtp-checkup BIN-DIR)))
