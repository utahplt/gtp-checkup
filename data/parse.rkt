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
    (parse-compile-timeout cfg-ln*)
    (parse-run-timeout cfg-ln*)
    'error))

(define ((make-parse-timeout before-str make-t) cfg-ln*)
  (define p-str (string-append "gtp-checkup: " before-str))
  (let loop ((cfg-ln* cfg-ln*))
    (cond
     [(null? cfg-ln*)
      #false]
     [(string-prefix? (car cfg-ln*) p-str)
      (if (null? (cdr cfg-ln*))
        (begin
          (printf "parse-timeout: text ended unexpectedly~n  context: ~s~n" cfg-ln*)
          #false)
        (let ((t-val (gtp-checkup-str->timeout (cadr cfg-ln*))))
          (and t-val (make-t t-val))))]
     [else
      (loop (cdr cfg-ln*))])))

(define parse-compile-timeout (make-parse-timeout "compiling" make-compile-timeout))
(define parse-run-timeout (make-parse-timeout "running" make-run-timeout))

(define (gtp-checkup-str->timeout str)
  (define m (regexp-match #rx"\\(([0-9]*)s\\)" str))
  (and m
    (let ()
      (define s (string->number (cadr m)))
      (unless (exact-nonnegative-integer? s)
        (raise-argument-error 'load-data "invalid timeout in line" str))
      s)))

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
     '#hash((acquire .  #hasheq((typed . (943 965 966 965 951 952 952 944 987 977)) (typed-worst-case .  (5843 5847 6128 5814 5865 5864 5835 5862 5863 5811)) (untyped . (420 420 417 422 432 434 421 429 429 434))))
            (dungeon .  #hasheq((typed . (110 109 110 111 110 111 110 110 110 110)) (typed-worst-case .  (69732 73825 71443 71680 69244 70136 69687 74691 71370 70119)) (untyped . (265 258 262 264 258 262 258 259 276 257))))
            (forth .  #hasheq((typed . (15 15 15 15 15 15 15 15 15 15)) (typed-worst-case . #s(timeout run 300)) (untyped . (12 13 12 12 13 12 12 12 12 13))))
            (fsm .  #hasheq((typed . (103 104 104 105 104 105 103 104 104 104)) (typed-worst-case .  (132666 130472 130292 131313 131234 131493 130057 130907 131466 130874)) (untyped . (201 200 200 200 200 200 201 199 201 202))))
            (fsmoo .  #hasheq((typed . (283 298 282 283 280 284 280 286 281 289)) (typed-worst-case . #s(timeout run 300)) (untyped . (261 264 261 266 262 263 264 266 260 261))))
            (gregor .  #hasheq((typed . (277 284 274 293 277 277 279 282 282 281)) (typed-worst-case . (424 429 464 459 433 431 439 433 423 443)) (untyped . (250 232 230 234 227 249 226 229 242 229))))
            (jpeg .  #hasheq((typed . error) (typed-worst-case . error) (untyped . error)))
            (kcfa .  #hasheq((typed . (1048 1071 1047 1049 1049 1050 1048 1048 1047 1048)) (typed-worst-case .  (6050 6009 5983 5974 5972 5980 5997 6001 5992 6043)) (untyped .  (1041 1031 1049 1029 1036 1094 1101 1055 1039 1040))))
            (lnm .  #hasheq((typed . (4857 4849 4969 4933 5075 4845 5081 4924 4821 5100)) (typed-worst-case .  (4997 4950 4937 4963 5059 4960 4957 5000 4924 5039)) (untyped . (904 906 917 930 908 943 906 913 908 911))))
            (mbta .  #hasheq((typed . (941 938 937 978 958 952 942 945 941 950)) (typed-worst-case .  (1092 1073 1087 1098 1097 1071 1084 1132 1113 1094)) (untyped . (608 605 609 600 603 603 602 603 604 603))))
            (morsecode .  #hasheq((typed . (1678 1660 1654 1654 1658 1656 1669 1655 1676 1655)) (typed-worst-case .  (2868 2816 2783 2689 2686 2683 2689 2672 2691 2707)) (untyped .  (1710 1716 1720 1712 1708 1713 1780 1711 1712 1709))))
            (quadT .  #hasheq((typed . error) (typed-worst-case . error) (untyped . error)))
            (quadU .  #hasheq((typed . (1600 1580 1582 1568 1758 1580 1595 1587 1562 1580)) (typed-worst-case .  (5580 5688 5645 5595 5617 5606 5606 5532 5563 5527)) (untyped .  (1164 1164 1166 1162 1169 1167 1170 1163 1162 1159))))
            (sieve .  #hasheq((typed . (2735 2726 2747 2719 2711 2740 2736 2715 2714 2705)) (typed-worst-case .  (15761 15710 16085 15793 15834 15695 15776 15781 15634 15805)) (untyped .  (2847 2831 2845 2865 2838 2852 2872 2853 2864 2853))))
            (snake .  #hasheq((typed . (597 557 556 554 554 559 563 556 556 556)) (typed-worst-case .  (8872 9040 9838 9280 9099 9241 9557 9394 9218 8964)) (untyped . (580 581 588 578 576 576 580 580 598 579))))
            (suffixtree .  #hasheq((typed . (1944 1944 1943 1968 1976 1950 1939 1961 1954 1943)) (typed-worst-case .  (77885 77652 77961 78243 78460 78700 79051 77585 78165 79245)) (untyped .  (2580 2611 2645 2577 2572 2592 2588 2576 2563 2597))))
            (synth .  #hasheq((typed . (403 390 389 410 396 393 391 392 431 391)) (typed-worst-case .  (25671 25369 25510 24535 25749 25550 25460 24949 26649 24830)) (untyped . (420 416 417 421 418 418 417 423 419 420))))
            (take5 .  #hasheq((typed . (4044 4070 3967 4012 4042 4003 3929 3977 3984 3959)) (typed-worst-case .  (105394 104350 107836 103754 107423 103925 104858 103285 105497 105283)) (untyped . (478 473 476 472 483 468 478 476 476 469))))
            (tetris .  #hasheq((typed . (726 726 726 728 730 726 727 733 727 728)) (typed-worst-case .  (8789 8550 8580 8344 8892 8475 8587 8623 8755 8479)) (untyped . (826 822 828 824 823 826 826 825 822 823))))
            (zombie .  #hasheq((typed . (218 218 217 210 212 216 211 207 209 208)) (typed-worst-case . #s(timeout run 300)) (untyped . (42 44 44 45 42 43 42 43 43 42))))
            (zordoz .  #hasheq((typed . (4046 4026 4036 4051 4129 4043 4017 4040 4021 4070)) (typed-worst-case .  (7332 7518 7368 7475 7292 7367 7430 7305 7239 7215)) (untyped . (836 821 828 827 822 844 825 822 831 828)))))))

  (test-case "load-directory"
    (define name "nsa")
    (define md (or (load-directory name)
                   (raise-user-error 'load-directory "failed to load '~a' directory" name)))
    (check-equal? (machine-data-id md) "nsa")
    (check-equal? (length (machine-data-commit* md))
                  (length (glob "nsa/*.txt"))))
)
