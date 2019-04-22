#lang racket/base

;; Tools for parsing raw data files.

(require racket/contract)
(provide
  (contract-out
    (directory->machine-name
      (-> path-string? string?))
    (directory->machine-uname
      (-> path-string? string?))
    (load-directory
      (-> directory-exists? (or/c machine-data? #f)))
    ;; Extract all datasets in a directory.
    (load-file
      (-> file-exists? benchmarks-data?))
    ;; Extract all benchmarks (and all configs) CPU times from a file
))

(require
  file/glob
  racket/path
  gtp-checkup/data/definition
  (only-in racket/file
    file->lines)
  (only-in racket/list
    index-where
    split-at
    take)
  racket/string
  (only-in gtp-util
    time-string->cpu-time
    path-string->string)
  )

(module+ test (require rackunit))

;; =============================================================================

(module+ test
  (test-case "commit-name?"
    (check-pred commit-name? "2017-01-24T12:30:46Z-0600_9078bc9efb081231f80dce6ab1939d8ba3cf112f")))

(define (load-directory src-dir)
  (define m-id (directory->machine-name src-dir))
  (define d*
    (for/list ((data-file (in-glob (build-path src-dir "*.txt")))
               #:when (commit-name? (path->name data-file)))
      (make-commit-data (path->name data-file) (load-file data-file))))
  (if (null? d*)
    #f
    (make-machine-data m-id d*)))

(define (directory->machine-name src-dir)
  (define-values [_base name _mbd] (split-path src-dir))
  (path-string->string name))

(define (directory->machine-uname src-dir)
  (define readme-str "README.md")
  (define readme (build-path src-dir readme-str))
  (unless (file-exists? readme)
    (raise-argument-error 'directory->machine-uname
                          (format "directory containing a '~a' file" readme-str)
                          src-dir))
  (with-input-from-file readme
    (lambda ()
      (void
        ;; Ignore everything before the uname section
        (let loop ()
          (unless (string-contains? (read-line) "uname -a")
            (loop))))
      (let loop ()
        (if (string-contains? (read-line) "```")
          (read-line)
          (loop))))))

(define (path->name pth)
  (path-string->string (path-replace-extension (file-name-from-path pth) "")))

(define (index-of-next-section ln*)
  (index-where ln* (lambda (ln) (string-prefix? ln "gtp-checkup: Checking"))))

(define (index-of-final-section ln*)
  (index-where ln* (lambda (ln) (string-prefix? ln "gtp-checkup: === FINISHED"))))

;; Split a file into a (Listof (Listof Line))
;;  where each inner list is the data for one configuration of one benchmark
(define (file->configuration-line* ps)
  (define-values [_pre* ln*]
    (let* ((all-ln* (file->lines ps))
           (fst-idx (index-of-next-section all-ln*)))
      (if fst-idx
        (split-at all-ln* fst-idx)
        (raise-arguments-error 'file->configuration-line*
                               "no matches for 'gtp-checkup: Checking', cannot split file into sections"
                               "file" ps))))
  (let loop ((ln* ln*))
    (cond
      [(index-of-next-section (cdr ln*))
       => (lambda (idx)
            (define-values [this-ln* next-ln*] (split-at ln* (+ 1 idx)))
            (cons this-ln* (loop next-ln*)))]
      [(index-of-final-section ln*)
       => (lambda (idx)
            (list (take ln* idx)))]
      [else
        (raise-arguments-error 'file->configuration-line*
                               "unexpected end of file, did not find '=== FINISHED ===' line"
                               "file" ps)])))

(define load-name
  (let ((rx-name #rx"^gtp-checkup: Checking 'benchmarks/([^/]*)/([^/]*)'"))
    (lambda (str)
      (define m (regexp-match rx-name str))
      (unless m
        (raise-argument-error 'load-name "string that begins with 'Checking ...'" str))
      (values (string->symbol (cadr m)) (string->symbol (caddr m))))))

(module+ test
  (test-case "load-name"
    (define (load-name* str)
      (define-values [a b] (load-name str))
      (cons a b))
    (check-equal? (load-name* "gtp-checkup: Checking 'benchmarks/acquire/typed'")
                  '(acquire . typed))
    (check-equal? (load-name* "gtp-checkup: Checking 'benchmarks/zordoz/untyped'")
                  '(zordoz . untyped))
    (check-equal? (load-name* "gtp-checkup: Checking 'benchmarks/suffixtree/typed-worst-case'")
                  '(suffixtree . typed-worst-case))))

(define (load-data cfg-ln*)
  ;; Either CPU times, or timeout, or error
  (define cpu*
    (for/list ((ln (in-list cfg-ln*))
               #:when (string-prefix? ln "cpu time:"))
      (time-string->cpu-time ln)))
  (or
    (and (not (null? cpu*)) cpu*)
    (for/or ((ln (in-list cfg-ln*))
             #:when (string-prefix? ln "gtp-checkup: exceeded time limit"))
      (define s (string->number (cadr (regexp-match #rx"\\(([0-9]*)s\\)" ln))))
      (unless (exact-nonnegative-integer? s)
        (raise-argument-error 'load-data "invalid timeout in line" ln))
      (cons 'timeout s))
    'error))

(define (hash->immutable-hash H)
  (for/hash (((k v) (in-hash H)))
    (values k v)))

(define (load-file ps)
  (define H (make-hasheq))
  (for ((cfg-ln* (in-list (file->configuration-line* ps))))
    (define-values [bm-name config-name] (load-name (car cfg-ln*)))
    (define t* (load-data (cdr cfg-ln*)))
    (hash-update! H bm-name
                  (lambda (h)
                    (hash-set h config-name t*))
                  (make-immutable-hasheq)))
  (hash->immutable-hash H))

;; -----------------------------------------------------------------------------

(module+ test
  (test-case "load-file"
    (check-equal?
     (load-file "nsa/2017-01-24T12:30:46Z-0600_9078bc9efb081231f80dce6ab1939d8ba3cf112f.txt")
     '#hash((acquire .  #hasheq((typed . (934)) (typed-worst-case . (5731)) (untyped . (426))))
            (dungeon .  #hasheq((typed . (110)) (typed-worst-case . (71818)) (untyped . (257))))
            (forth .  #hasheq((typed . (15)) (typed-worst-case . (timeout . 300)) (untyped . (13))))
            (fsm .  #hasheq((typed . (104)) (typed-worst-case . (130341)) (untyped . (199))))
            (fsmoo .  #hasheq((typed . (316)) (typed-worst-case . (timeout . 300)) (untyped . (261))))
            (gregor .  #hasheq((typed . (289)) (typed-worst-case . (437)) (untyped . (269))))
            (jpeg .  #hasheq((typed . error) (typed-worst-case . error) (untyped . error)))
            (kcfa .  #hasheq((typed . (1046)) (typed-worst-case . (6169)) (untyped . (1023))))
            (lnm .  #hasheq((typed . (4863)) (typed-worst-case . (4810)) (untyped . (900))))
            (mbta .  #hasheq((typed . error) (typed-worst-case . (1068)) (untyped . (601))))
            (morsecode .  #hasheq((typed . (1654)) (typed-worst-case . (2698)) (untyped . (1714))))
            (quadT .  #hasheq((typed . (timeout . 60)) (typed-worst-case . error) (untyped . error)))
            (quadU .  #hasheq((typed . error) (typed-worst-case . (6151)) (untyped . (1159))))
            (sieve .  #hasheq((typed . (2739)) (typed-worst-case . (15889)) (untyped . (2827))))
            (snake .  #hasheq((typed . (557)) (typed-worst-case . (9104)) (untyped . (579))))
            (suffixtree .  #hasheq((typed . (1950)) (typed-worst-case . (77660)) (untyped . (2601))))
            (synth .  #hasheq((typed . (timeout . 60)) (typed-worst-case . (25813)) (untyped . (428))))
            (take5 .  #hasheq((typed . (4019)) (typed-worst-case . (105367)) (untyped . (478))))
            (tetris .  #hasheq((typed . (727)) (typed-worst-case . (8332)) (untyped . (826))))
            (zombie .  #hasheq((typed . (214)) (typed-worst-case . (timeout . 300)) (untyped . (43))))
            (zordoz .  #hasheq((typed . error) (typed-worst-case . (7472)) (untyped . (815)))))))

  (test-case "load-directory"
    (define name "nsa")
    (define md (or (load-directory name)
                   (raise-user-error 'load-directory "failed to load '~a' directory" name)))
    (check-equal? (machine-data-id md) "nsa")
    (check-equal? (length (machine-data-commit* md))
                  (length (glob "nsa/*.txt"))))
)
