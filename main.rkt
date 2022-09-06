#lang racket/base

(require racket/contract)
(provide
  racket-bin-dir/c
  gtp-checkup-logger
  (contract-out
    (gtp-checkup
     (->* [racket-bin-dir/c]
          [#:iterations (or/c #f exact-positive-integer?)
           #:timeout (or/c #f (cons/c exact-positive-integer? exact-positive-integer?))]
          void?))
    (import-benchmark
     (-> directory-exists? void?))))

(require
  file/glob
  gtp-checkup/private/logger
  (only-in racket/file
    make-temporary-file
    delete-directory/files
    copy-directory/files)
  racket/path
  (only-in racket/sandbox
    exn:fail:resource?
    with-deep-time-limit)
  (only-in racket/string
    string-replace
    string-join)
  (only-in racket/system
    system*)
  racket/runtime-path)

;; =============================================================================

(define-runtime-path PWD ".")

(define COMPILE-TIME-LIMIT (* 60 10)) ; seconds
(define RUN-TIME-LIMIT     (* 60 5)) ; seconds

(define (contains-racket? d)
  (file-exists? (build-path d "racket")))

(define (contains-raco? d)
  (file-exists? (build-path d "raco")))

(define racket-bin-dir/c
  (and/c directory-exists?
         contains-racket?
         contains-raco?))

(define BASE "base")
(define BENCHMARKS "benchmarks")
(define BOTH "both")
(define TWC "typed-worst-case")
(define TYPED "typed")
(define UNTYPED "untyped")

(define SEARCH-FOR-FILES-MATCHING "benchmarks/*/*/main.rkt")

(define DEFAULT-NUM-ITERATIONS 4)

;; -----------------------------------------------------------------------------

(define (gtp-checkup bin-dir
                     #:iterations [pre-iters #f]
                     #:timeout [pre-timeout #f])
  (define iterations (or pre-iters DEFAULT-NUM-ITERATIONS))
  (define timeout (or pre-timeout (cons COMPILE-TIME-LIMIT RUN-TIME-LIMIT)))
  (define results
    (parameterize ([current-directory PWD])
      (for/list ((main (in-glob SEARCH-FOR-FILES-MATCHING)))
        (cons main (checkup-file bin-dir main iterations timeout)))))
  (log-gtp-checkup-info "=== FINISHED ===")
  (print-summary results))

(define (checkup-file bin-dir main.rkt iters time*)
  (define-values [dir name _] (split-path main.rkt))
  (define-values [compile-time-limit run-time-limit] (values (car time*) (cdr time*)))
  (define rel-dir (find-relative-path (current-directory) dir))
  (log-gtp-checkup-info "Checking '~a'" rel-dir)
  (parameterize ([current-directory dir])
    (and (delete-compiled)
         (log-gtp-checkup-info "compiling '~a'" name)
         (raco-make bin-dir name compile-time-limit)
         (log-gtp-checkup-info "running '~a'" name)
         (for/and ((i (in-range iters)))
           (run-racket bin-dir name run-time-limit))
         #true)))

(define ((handle-resource-failure time-limit) e)
  (log-gtp-checkup-error "exceeded time limit (~as)" time-limit)
  #f)

(define (raco-make bin name time-limit)
  (shell #:time-limit time-limit (build-path bin "raco") "make" name))

(define (run-racket bin name time-limit)
  (shell #:time-limit time-limit (build-path bin "racket") name))

(define (raco-setup bin name)
  (shell* (build-path bin "raco") (list "setup" name)))

(define (delete-compiled)
  (delete-directory/files "compiled" #:must-exist? #f))

(define (shell cmd #:time-limit [time-limit RUN-TIME-LIMIT] . arg*)
  (define success (box #f))
  (with-handlers ([exn:fail:resource? (handle-resource-failure time-limit)])
    (with-deep-time-limit time-limit
      (set-box! success (shell* cmd arg*)))
    (unbox success)))

(define (shell* cmd arg*)
  (apply system* cmd arg*))

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

;; import-benchmark : (-> directory-exists? void?)
;; Copies files from the given directory to create a new checkup benchmark.
;; The checkup version has ONLY the typed configuration, and uses
;;  require/typed instead of require/typed/check to put a boundary between
;;  every pair of modules.
(define (import-benchmark bm-dir)
  (define not-gtp (why-not-gtp-dir bm-dir))
  (cond
   [not-gtp
    (raise-user-error 'import-benchmark "failed to import directory '~a': ~a" bm-dir not-gtp)]
   [else
    (import/gtp-dir bm-dir)]))

(define (import/gtp-dir src-dir)
  (define program-name (directory-name-from-path src-dir))
  (define typed-dir (build-path src-dir TYPED))
  (define untyped-dir (build-path src-dir UNTYPED))
  (define copy-base-dir
    (make-copy-dir (build-path src-dir BASE)))
  (define copy-both-dir
    (make-copy-dir (build-path src-dir BOTH)))
  (define file* (racket-files typed-dir))
  (define new-dir (build-path PWD BENCHMARKS program-name))
  (void
    (make-directory new-dir))
  (copy-base-dir (build-path new-dir BASE))
  (let ([new-u-dir (build-path new-dir UNTYPED)])
    (copy-directory/files untyped-dir new-u-dir)
    (copy-both-dir new-u-dir))
  (let ([new-t-dir (build-path new-dir TYPED)])
    (copy-directory/files typed-dir new-t-dir)
    (copy-both-dir new-t-dir))
  (let ([new-twc-dir (build-path new-dir TWC)])
    (copy-directory/files typed-dir new-twc-dir)
    (copy-both-dir new-twc-dir)
    (remove-require-typed-check new-twc-dir))
  (log-gtp-checkup-info "finished importing '~a', please check that everything looks good and that 'main.rkt' runs" program-name)
  (void))

(define (make-copy-dir d)
  (if (directory-exists? d)
    (λ (dest)
      (for ([src (in-glob (build-path d "*"))])
        (define name (file-name-from-path src))
        (unless (directory-exists? dest)
          (make-directory dest))
        (copy-directory/files src (build-path dest name))))
    void))

(define (directory-name-from-path path)
  (define-values [_base name _mbd] (split-path path))
  (and (path? name) name))

(define (gtp-dir? dir)
  (not (why-not-gtp-dir dir)))

;; Return a string explaining why `dir` is NOT a proper benchmark directory,
;;  or `#false` if the directory is perfectly acceptable.
(define (why-not-gtp-dir dir)
  (define program-name (directory-name-from-path dir))
  (define typed-dir (build-path dir TYPED))
  (define untyped-dir (build-path dir UNTYPED))
  (cond
   [(not program-name)
    "could not parse directory name"]
   [(directory-exists? (build-path PWD BENCHMARKS program-name))
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

(define (remove-require-typed-check dir)
  (for ((fn (in-glob (build-path dir "*.rkt"))))
    (remove-require-typed-check/file fn)))

(define (remove-require-typed-check/file fn)
  (define tmp (make-temporary-file))
  (with-output-to-file tmp #:exists 'append
    (lambda ()
      (with-input-from-file fn
        (lambda ()
          (for ((ln (in-lines)))
            (displayln (string-replace* ln '(("require-typed-check" " ") ("require/typed/check" "require/typed")))))))))
  (copy-file tmp fn #true)
  (void))

(define (string-replace* str ft* #:all? [all? #true])
  (for/fold ((acc str))
            ((ft (in-list ft*)))
    (string-replace acc (car ft) (cadr ft) #:all? all?)))

;; =============================================================================

(module* main racket/base
  (require racket/cmdline (submod ".."))
  (define program-name "gtp-checkup")
  (define cmd-mode (box 'checkup))
  (define new-iters (box #false))
  (define new-timeout (box #false))
  (define (parse-iters str)
    (define i-val (string->number str))
    (if (or (not i-val) (< i-val 0))
      (raise-argument-error (string->symbol program-name) "exact-positive-integer?" str)
      i-val))
  (define (parse-timeout str)
    (define t-val (string->number str))
    (if (or (not t-val) (< 0 t-val))
      t-val
      (raise-argument-error (string->symbol program-name) "(or/c #false exact-positive-integer?)" str)))
  (command-line
   #:program program-name
   #:once-any
   [("-n" "--new" "--import") "Import a new program" (set-box! cmd-mode 'import)]
   [("-i" "--iters") i-str "Number of times to run each configuration" (set-box! new-iters (parse-iters i-str))]
   [("--time-limit") t-str "Max seconds to wait for a configuration to compile or run, or #false" (set-box! new-timeout (parse-timeout t-str))]
   #:args (BIN-DIR)
   (case (unbox cmd-mode)
    [(import)
     (import-benchmark BIN-DIR)]
    [(checkup)
     (define t
       (let ((v (unbox new-timeout)))
         (if v (cons v v) v)))
     (gtp-checkup BIN-DIR #:iterations (unbox new-iters) #:timeout t)]
    [else
     (raise-user-error 'gtp-checkup "unknown mode '~a', goodbye" (unbox cmd-mode))])))

;; -----------------------------------------------------------------------------

(module+ test
  (require compiler/find-exe)
  (let* ((racket-exe (find-exe))
         (bin-dir (path-only racket-exe)))
    (unless bin-dir
      (raise-user-error 'gtp-checkup "failed to find racket/bin/ folder because (find-exe) returned ~s" racket-exe))
    (unless (putenv "PLTSTDERR" "error info@gtp-checkup")
      (log-gtp-checkup-error "failed to update PLTSTDERR environment variable, going to run anyway"))
    (gtp-checkup bin-dir)))
