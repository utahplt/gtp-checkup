#lang racket/base

;; TODO
;; - time & space limits for 'raco' and 'racket'
;; - quickstart for adding a benchmark

(require racket/contract)
(provide
  (contract-out
    (gtp-checkup
     (-> racket-bin-dir/c void?))
    (import-benchmark
     (-> directory-exists? void?))))

(require
  file/glob
  racket/path
  (only-in racket/string
    string-join)
  (only-in racket/system
    system*)
  racket/runtime-path)

;; =============================================================================

(define-logger gtp-checkup)

(define-runtime-path PWD ".")

(define SEARCH-FOR-FILES-MATCHING "**/main.rkt")

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
  (define results
    (parameterize ([current-directory PWD])
      (for/list ((main (in-glob SEARCH-FOR-FILES-MATCHING)))
        (cons main (checkup-file bin-dir main)))))
  (log-gtp-checkup-info "=== FINISHED ===")
  (print-summary results))

(define (checkup-file bin-dir main.rkt)
  (define-values [dir name _] (split-path main.rkt))
  (define rel-dir (find-relative-path (current-directory) dir))
  (log-gtp-checkup-info "Checking '~a'" rel-dir)
  (parameterize ([current-directory dir])
    (and (log-gtp-checkup-info "compiling '~a'" name)
         (raco-make bin-dir name)
         (log-gtp-checkup-info "running '~a'" name)
         (run-racket bin-dir name)
         #true)))

(define (raco-make bin name)
  (system* (build-path bin "raco") "make" name))

(define (run-racket bin name)
  (system* (build-path bin "racket") name))

(define (print-summary results)
  (define cwd (current-directory))
  (define failed-to-run
    (for/list ([r (in-list results)]
               #:when (not (cdr r)))
      (path-string->string (find-relative-path cwd (car r)))))
  (if (null? failed-to-run)
    (log-gtp-checkup-info "All benchmarks finished successfully")
    (begin
      (log-gtp-checkup-info "Error running files:")
      (for ([fail (in-list failed-to-run)])
        (log-gtp-checkup-info "- ~a" fail)))))

(define (path-string->string x)
  (if (path? x) (path->string x) x))

;; -----------------------------------------------------------------------------

(define (import-benchmark bm-dir)
  (cond
   [(gtp-dir? bm-dir)
    (void)]
   [else
    (raise-user-error 'import-benchmark "failed to import directory '~a', unrecognized directory structure")]))

(define (gtp-dir? dir)
  #false)

;; =============================================================================

(module* main racket/base
  (require racket/cmdline (submod ".."))
  (define cmd-mode (box 'checkup))
  (command-line
   #:program "gtp-checkup"
   #:once-any
   [("-n" "--new") "Import a new program" (set-box! cmd-mode 'import)]
   #:args (BIN-DIR)
   (case (unbox cmd-mode)
    [(import)
     (import-benchmark BIN-DIR)]
    [(checkup)
     (gtp-checkup BIN-DIR)]
    [else
     (raise-user-error 'gtp-checkup "unknown mode '~a', goodbye" (unbox cmd-mode))])))
