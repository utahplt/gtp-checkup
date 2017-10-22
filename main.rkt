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
  (only-in racket/file
    copy-directory/files)
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
      (log-gtp-checkup-info "Error running the following files:")
      (for ([fail (in-list failed-to-run)])
        (log-gtp-checkup-info "- ~a" fail)))))

(define (path-string->string x)
  (if (path? x) (path->string x) x))

;; -----------------------------------------------------------------------------

(define (import-benchmark bm-dir)
  (define not-gtp (why-not-gtp-dir bm-dir))
  (cond
   [not-gtp
    (raise-user-error 'import-benchmark "failed to import directory '~a': ~a" bm-dir not-gtp)]
   [else
    (import/gtp-dir bm-dir)]))

(define (import/gtp-dir dir)
  (define program-name (directory-name-from-path dir))
  (define typed-dir (build-path dir "typed"))
  (define untyped-dir (build-path dir "untyped"))
  (define base-dir (build-path dir "base"))
  (define copy-both-dir
    (let ([d (build-path dir "both")])
      (if (directory-exists? d)
        (λ (dest)
          (for ([src (in-glob (build-path d "*"))])
            (define name (file-name-from-path src))
            (copy-directory/files src (build-path dest name))))
        void)))
  (define file* (racket-files typed-dir))
  (define configuration (make-vector (length file*) 0))
  (define new-dir (build-path PWD program-name))
  (make-directory new-dir)
  (when (directory-exists? base-dir)
    (copy-directory/files base-dir (build-path new-dir "base")))
  (let ([u-dir (build-path new-dir "untyped")])
    (copy-directory/files untyped-dir u-dir)
    (copy-both-dir u-dir))
  (let ([t-dir (build-path new-dir "typed")])
    (copy-directory/files typed-dir t-dir)
    (copy-both-dir t-dir))
  (for ([typed-file (in-list file*)]
        [i (in-naturals)])
    (define dirname (build-path new-dir (format "gradual-~a" i)))
    (copy-directory/files untyped-dir dirname)
    (copy-both-dir dirname)
    (copy-file (build-path typed-dir typed-file) (build-path dirname typed-file) #true))
  (log-gtp-checkup-info "finished importing '~a', please manually remove dependencies" program-name)
  (void))

(define (directory-name-from-path path)
  (define-values [_base name _mbd] (split-path path))
  (and (path? name) name))

(define (gtp-dir? dir)
  (not (why-not-gtp-dir dir)))

;; Return a string explaining why `dir` is NOT a proper benchmark directory,
;;  or `#false` if the directory is perfectly acceptable.
(define (why-not-gtp-dir dir)
  (define program-name (directory-name-from-path dir))
  (define typed-dir (build-path dir "typed"))
  (define untyped-dir (build-path dir "untyped"))
  (cond
   [(not program-name)
    "could not parse directory name"]
   [(directory-exists? (build-path PWD program-name))
    "program already exists"]
   [(not (directory-exists? typed-dir))
    "missing 'typed/' directory"]
   [(not (directory-exists? untyped-dir))
    "missing 'untyped/' directory"]
   [(not (equal? (racket-files typed-dir) (racket-files untyped-dir)))
    "different '*.rkt' files in typed/ and untyped/ directories"]
   [else
    #false]))

(define (racket-files dir)
  (define filename*
    (for/list ([full-path (in-glob (build-path dir "*.rkt"))])
      (path-string->string (file-name-from-path full-path))))
  (sort filename* string<=?))

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
