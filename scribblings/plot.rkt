#lang racket/base

;; Tools for plotting parsed data.
;; See `data/` for raw data and `data/parse.rkt` for parsing tools.

;; TODO
;; - make scribble page with machine name + specs at top
;; - 

(provide
  )

(require
  (only-in racket/math order-of-magnitude)
  (only-in math/statistics mean)
  (only-in racket/string string-split)
  (only-in racket/path shrink-path-wrt)
  (only-in gtp-util path-string->string)
  (only-in gregor ->posix parse-datetime ->year datetime datetime<?)
  (rename-in gregor [datetime<=? datetime<=?2])
  gtp-checkup/data/definition
  gtp-checkup/data/parse
  racket/generator
  racket/runtime-path
  racket/sequence
  pict-abbrevs
  pict
  (only-in plot/utils ->pen-color)
  plot/no-gui)

(module+ test (require rackunit racket/set))

;; =============================================================================

(define *point-outline-color* (make-parameter "DimGray"))
(define *wide-plot-width* (make-parameter 800))

(define *year-rule-color* (make-parameter (->pen-color 0)))
(define *year-rule-width* (make-parameter 1))
(define *year-rule-alpha* (make-parameter 0.6))

(define *release-rule-color* (make-parameter (->pen-color 0)))
(define *release-rule-width* (make-parameter 10))
(define *release-rule-alpha* (make-parameter 0.14))

(define day-seconds (* 60 60 60 24))

(define racket-release-time*
  (list (datetime 2016 10 26) ;; 6.7
        (datetime 2017 01 24)
        (datetime 2017 04 27)
        (datetime 2017 07 31)
        (datetime 2017 09 12)
        (datetime 2017 10 30)
        (datetime 2018 01 26)
        (datetime 2018 07 27)
        (datetime 2018 10 26)
        (datetime 2019 01 30)))

(define change-type* '(slower faster new-fail new-fix still-fail))

;; -----------------------------------------------------------------------------

(define (make-machine-data-pict* md)
  (define m-id (machine-data-id md))
  (define benchmark-name* (machine-data->benchmark-name* md))
  (parameterize (;; TODO possible to (1) set defaults (2) let users override (3) don't define new parameters like gtp-plot does?
                 [plot-x-ticks (date-ticks #:formats '("~m/~d"))]
                 [plot-width (*wide-plot-width*)]
                 [point-alpha 0.8]
                 [point-size (*release-rule-width*)]
                 [plot-font-size 18]
                 [plot-font-family 'default])
    (for/list ([b-id (in-list benchmark-name*)])
      (define *max-cpu-time (box 0))
      (define *min-time (box #f))
      (define *max-time (box #f))
      (define renderer*
        (for/list ((cfg (in-list configuration-name*)))
          ;; cfg = a dataset ... one group of points
          (define cfg-color (configuration-name->color cfg))
          (define p* ;; collect all, find max/min statistics
            (for/list ((cd (in-list (machine-data-commit* md))))
              (define commit-seconds
                (let ((ctime (commit-id->time (commit-data-id cd)))
                      (old-min (unbox *min-time))
                      (old-max (unbox *max-time)))
                  (when (or (not old-min) (datetime<? ctime old-min))
                    (set-box! *min-time ctime))
                  (when (or (not old-max) (datetime<? old-max ctime))
                    (set-box! *max-time ctime))
                  (->posix ctime)))
              (define r-val
                (let* ((r-val (hash-ref (hash-ref (commit-data-benchmark# cd) b-id) cfg))
                       (n (result->natural r-val 0)))
                  (when (and n (< (unbox *max-cpu-time) n))
                    (set-box! *max-cpu-time n))
                  r-val))
              (cons commit-seconds r-val)))
          (define max-cpu-time (unbox *max-cpu-time))
          (define (point->plot-point p)
            ;; slight misnomer, because `p` might be a valid plot point --- if the `cdr` is right
            (vector (car p) (result->natural (cdr p) max-cpu-time)))
          (define point-renderer*
            (filter
              values
              (for/list ((r-kind (in-list '(ok error timeout))))
                (define p*/kind
                  (for/list ((p (in-list p*))
                             #:when (eq? r-kind (result->kind (cdr p))))
                    (point->plot-point p)))
                (and (not (null? p*/kind))
                     (points p*/kind
                             #:color (*point-outline-color*)
                             #:fill-color cfg-color
                             #:sym (kind->symbol r-kind))))))
          (define line-renderer*
            (for/list ((dt (in-list change-type*)))
              (define plot-line-seg? (change-type->predicate dt))
              (define width (change-type->width dt))
              (define alpha (change-type->alpha dt))
              (for/list ((pp (in-pairs p*))
                         #:when (plot-line-seg? pp))
                (lines
                  (vector (point->plot-point (car pp))
                          (point->plot-point (cdr pp)))
                  #:color cfg-color
                  #:width width
                  #:alpha alpha))))
          (list line-renderer* point-renderer*)))
      (define-values [max-cpu-time min-time max-time]
        (values (unbox *max-cpu-time) (unbox *min-time) (unbox *max-time)))
      (define time-padding day-seconds)
      (define the-plot
        (plot-pict
          (list (make-release-renderer* min-time max-time)
                (make-year-renderer* min-time max-time)
                renderer*)
          #:x-min (- (->posix min-time) time-padding)
          #:x-max (+ (->posix max-time) time-padding)
          #:y-min 0
          #:y-max (+ max-cpu-time (expt 10 (- (order-of-magnitude max-cpu-time) 1)))
          #:width (plot-width)
          #:height (* 3/4 (plot-width))
          #:title (format "~a" b-id)
          #:x-label "commit date"
          #:y-label "runtime (seconds)"))
      (hb-append 10 the-plot (make-cfg-color-legend)))))

(define (make-cfg-color-legend)
  (add-rectangle-background
    #:radius 0
    #:x-margin 10
    #:y-margin 10
    (table 2
           (apply append
             (for/list ((nm (in-list configuration-name*)))
               (list (filled-rounded-rectangle 18 10
                                               #:draw-border? #false
                                               #:color (configuration-name->color nm))
                     (text (symbol->string nm) (plot-font-family) (plot-font-size)))))
           lc-superimpose
           lc-superimpose
           10
           8)))

(define (make-year-renderer* min-time max-time)
  (for/list ((y (in-range (->year min-time) (+ 1 (->year max-time)))))
    (vrule (->posix (datetime y))
           #:color (*year-rule-color*)
           #:width (*year-rule-width*)
           #:alpha (*year-rule-alpha*))))

(define (datetime<=? . t*)
  (let loop ((t* t*))
    (if (or (null? t*) (null? (cdr t*)))
      #true
      (and (datetime<=?2 (car t*) (cadr t*)) (loop (cdr t*))))))

(define (make-release-renderer* min-time max-time)
  (for/list ((r (in-list racket-release-time*))
             #:when (datetime<=? min-time r max-time))
    (vrule (->posix r)
           #:color (*release-rule-color*)
           #:width (*release-rule-width*)
           #:alpha (*release-rule-alpha*))))

(define (commit-id->time cid)
  ;; posix = seconds since UNIX epoch
  (define timestamp (car (string-split cid "_")))
  (parse-datetime timestamp "yyyy-MM-dd'T'HH:mm:ss'Z'xx"))

(module+ test
  (test-case "commit-id->time"
    (check-equal? (->posix (commit-id->time "2018-10-26T14:51:55Z-0500_3475e86862a6fd5389ff5f22c456107c74fd05c5"))
                  1540565515)))

(define (list-insert x* x <=?)
  (let loop ((x* x*))
    (cond
      [(null? x*)
       (list x)]
      [(<=? x (car x*))
       (if (<=? (car x*) x)
         x* ;; `x` is a duplicate item
         (cons x x*))]
      [else
        (cons (car x*) (loop (cdr x*)))])))

(define (machine-data->benchmark-name* md)
  ;; TODO check that all datasets agree on the names
  (for*/fold ((acc '()))
             ((cd (in-list (machine-data-commit* md)))
              (k (in-hash-keys (commit-data-benchmark# cd))))
    (list-insert acc k symbol<=?)))

(define (symbol<=? a b)
  (or (eq? a b)
      (symbol<? a b)))

(define (in-machine-data-result md)
  (for*/list ((cmt (in-list (machine-data-commit* md)))
         (cfg# (in-hash-values (commit-data-benchmark# cmt)))
         (r  (in-hash-values cfg#)))
    r))

(define (machine-data->timeout* md)
  (for*/list ((r (in-machine-data-result md))
              #:when (timeout? r))
    (timeout->time-limit r)))

(module+ test
  (define sample-md
    (make-machine-data
      "M"
      (list
        (make-commit-data
          "2018-10-26T14:51:55Z-0500_3475e86862a6fd5389ff5f22c456107c74fd05c5"
          '#hasheq(
            (synth . #hasheq((typed . (timeout . 60)) (untyped . (428))))
            (take5 . #hasheq((typed . (4019))
                             (typed-worst-case . (timeout . 103))
                             (untyped . (478)))))))))
  (test-case "machine-data->timeout*"
    (check set=?
           (machine-data->timeout* sample-md) '(60 103))))

(define (machine-data->cpu-time* md)
  (for*/list ((r (in-machine-data-result md))
              #:when (cpu-time*? r)
              (n (in-list r)))
    n))

(module+ test
  (test-case
    "machine-data->cpu-time*"
    (check set=?
           (machine-data->cpu-time* sample-md)
           '(428 4019 478))))

(define (kind->symbol x)
  (case x
    ((ok)
     'fullcircle)
    ((error)
     'times)
    ((timeout)
     'fulltriangledown)))

(define configuration-name->color
  (let ((H #hasheq((typed . "Gold")
                   (untyped . "Plum")
                   (typed-worst-case . "Cadet Blue"))))
    (lambda (n)
      (hash-ref H n
                (lambda () (raise-argument-error 'configuration-name->color "configuration-name?" n))))))

(define (result->kind x)
  (cond
    [(cpu-time*? x)
     'ok]
    [(timeout? x)
     'timeout]
    [else
      'error]))

(define (result->natural x n-timeout)
  (cond
    [(cpu-time*? x)
     (/ (mean x) 1000)]
    [(timeout? x)
     n-timeout]
    [else
     n-timeout]))

(define (in-pairs orig-x*)
  (define *x* (box orig-x*))
  (in-producer
    (lambda ()
      (define x* (unbox *x*))
      (if (or (null? x*) (null? (cdr x*)))
        #f
        (begin0
          (cons (car x*) (cadr x*))
          (set-box! *x* (cdr x*)))))
    #f))

(module+ test
  (test-case "in-pairs"
    (check-equal? (sequence->list (in-pairs '()))
                  '())
    (check-equal? (sequence->list (in-pairs '(1)))
                  '())
    (check-equal? (sequence->list (in-pairs '(1 2)))
                  '((1 . 2)))
    (check-equal? (sequence->list (in-pairs '(1 2 3)))
                  '((1 . 2) (2 . 3)))))

(define (change-type->predicate dt)
  (define (points->results pp)
    (values (cdr (car pp))
            (cdr (cdr pp))))
  (define (make-cpu-time-pred cmp)
    (lambda (pp)
      (define-values [fst snd] (points->results pp))
      (and (eq? 'ok (result->kind fst))
           (eq? 'ok (result->kind snd))
           (cmp (result->natural fst #f) (result->natural snd #f)))))
  (case dt
    ((slower)
     (make-cpu-time-pred <))
    ((faster)
     (make-cpu-time-pred >=))
    ((new-fail)
     (lambda (pp)
       (define-values [fst snd] (points->results pp))
       (and (eq? 'ok (result->kind fst))
            (not (eq? 'ok (result->kind snd))))))
    ((new-fix)
     (lambda (pp)
       (define-values [fst snd] (points->results pp))
       (and (not (eq? 'ok (result->kind fst)))
            (eq? 'ok (result->kind snd)))))
    ((still-fail)
     (lambda (pp)
       (define-values [fst snd] (points->results pp))
       (and (not (eq? 'ok (result->kind fst)))
            (not (eq? 'ok (result->kind snd))))))
    (else
      (raise-argument-error 'change-type->predicate "change-type?" dt))))

(define (change-type->color dt)
  (case dt
    ((slower)
     "DarkOrange")
    ((faster)
     "LimeGreen")
    ((new-fail)
     "Crimson")
    ((new-fix)
     "LimeGreen")
    ((still-fail)
     "Black")
    (else
      (raise-argument-error 'change-type->color "change-type?" dt))))

(define (change-type->width dt)
  (case dt
    ((slower faster new-fix still-fail)
     3)
    ((new-fail)
     3)
    (else
      (raise-argument-error 'change-type->width "change-type?" dt))))

(define (change-type->alpha dt)
  (case dt
    ((slower faster new-fix still-fail)
     0.4)
    ((new-fail)
     0.9)
    (else
      (raise-argument-error 'change-type->alpha "change-type?" dt))))

;; =============================================================================

(module+ main
  (define-runtime-path data-dir "../data/")
  #;(define d (load-directory (build-path data-dir "nsa")))
  #;(save-pict "nsa.png"
             (apply vl-append 20 (make-machine-data-pict* d)))
  (define aquire-data
    '#s(machine-data
         "/path/to/../data/nsa"
         (#s(commit-data "2017-01-24T12:30:46Z-0600_9078bc9efb081231f80dce6ab1939d8ba3cf112f"
                         #hasheq((acquire . #hasheq((typed . (934)) (typed-worst-case . (5731)) (untyped . (426))))))
          #s(commit-data "2017-04-27T09:21:16Z-0500_70348f2b84459a5eb3e8feaa737e70028ece0747"
                         #hasheq((acquire . #hasheq((typed . (961)) (typed-worst-case . (5858)) (untyped . (439))))))
          #s(commit-data "2018-07-27T10:22:15Z-0500_00f2b69e22f7f5bf87c43fb2513fd8f2da269cef"
                         #hasheq((acquire . #hasheq((typed . (1066)) (typed-worst-case . (2971)) (untyped . (469))))))
          #s(commit-data "2018-10-26T14:51:55Z-0500_3475e86862a6fd5389ff5f22c456107c74fd05c5"
                         #hasheq((acquire . #hasheq((typed . (984)) (typed-worst-case . (2916)) (untyped . (462))))))
          #s(commit-data "2019-01-30T09:19:22Z-0600_5bf83b8ef26856bc473eaf74fc8ee4813e167f9e"
                         #hasheq((acquire . #hasheq((typed . (843)) (typed-worst-case . (1892)) (untyped . (462))))))
          #s(commit-data "2019-02-25T13:10:08Z-0700_84837f4330cef3df9271b778f2fbfba09d34fc3b"
                         #hasheq((acquire . #hasheq((typed . (854 843 857 875 852 861 873 854 837 847)) (typed-worst-case . (1901 1895 1864 1887 1859 1878 1923 1877 1873 1892)) (untyped . (469 462 460 453 462 466 464 464 477 462))))))
          #s(commit-data "2019-03-15T22:13:10Z-0500_ce324be9f8b8ad8b88bc3a39e7b1de438b462c87"
                         #hasheq((acquire . #hasheq((typed . (865 866 869 854 848 852 864 847 847 838)) (typed-worst-case . (1889 1898 1923 1852 1876 1893 1917 1869 1863 1886)) (untyped . (486 457 467 466 469 462 470 476 464 467))))))
          #s(commit-data "2019-03-16T17:11:55Z-0400_a2d87c353eb3ae6431a91a2e924c2216756ff079"
                         #hasheq((acquire . #hasheq((typed . (840 860 847 860 858 868 840 865 841 840)) (typed-worst-case . (1876 1952 1893 1895 1905 1869 1963 1883 1882 1937)) (untyped . (470 469 471 473 482 468 480 462 465 470))))))
          #s(commit-data "2019-03-17T07:04:23Z-0500_ed2381ee595fa8ac06dded9aacaa4c34f5d73475"
                         #hasheq((acquire . #hasheq((typed . (857 865 871 851 842 854 852 845 872 859)) (typed-worst-case . (1902 1923 1884 1944 1892 1884 1922 1886 1901 1899)) (untyped . (467 461 470 462 473 466 468 466 474 475))))))
          #s(commit-data "2019-03-28T17:08:25Z-0500_7a9b1d065e168d882ac8800e3fed4340c940e3ae"
                         #hasheq((acquire . #hasheq((typed . (858 834 842 859 848 848 848 845 851 847)) (typed-worst-case . (1924 1940 1743 1927 1862 1883 1888 1918 1915 1902)) (untyped . (470 475 473 468 478 461 467 473 464 461))))))
          #s(commit-data "2019-03-28T17:08:25Z-0500_e1835074f5c44581cb9645f11f7ca8096e61a546"
                         #hasheq((acquire . #hasheq((typed . (853 863 847 845 861 848 862 851 869 846)) (typed-worst-case . (1907 1911 1910 1883 1934 1916 1886 1941 1867 1905)) (untyped . (472 470 455 470 469 454 478 470 461 465))))))
          #s(commit-data "2017-01-24T12:30:46Z-0600_9078bc9efb081231f80dce6ab1939d8ba3cf112f"
                         #hasheq((acquire . #hasheq((typed . (934)) (typed-worst-case . (5731)) (untyped . (426))))))
          #s(commit-data "2017-04-27T09:21:16Z-0500_70348f2b84459a5eb3e8feaa737e70028ece0747"
                         #hasheq((acquire . #hasheq((typed . (961)) (typed-worst-case . (5858)) (untyped . (439))))))
          #s(commit-data "2018-07-27T10:22:15Z-0500_00f2b69e22f7f5bf87c43fb2513fd8f2da269cef"
                         #hasheq((acquire . #hasheq((typed . (1066)) (typed-worst-case . (2971)) (untyped . (469))))))
          #s(commit-data "2018-10-26T14:51:55Z-0500_3475e86862a6fd5389ff5f22c456107c74fd05c5"
                         #hasheq((acquire . #hasheq((typed . (984)) (typed-worst-case . (2916)) (untyped . (462))))))
          #s(commit-data "2019-01-30T09:19:22Z-0600_5bf83b8ef26856bc473eaf74fc8ee4813e167f9e"
                         #hasheq((acquire . #hasheq((typed . (843)) (typed-worst-case . (1892)) (untyped . (462))))))
          #s(commit-data "2019-02-25T13:10:08Z-0700_84837f4330cef3df9271b778f2fbfba09d34fc3b"
                         #hasheq((acquire . #hasheq((typed . (854 843 857 875 852 861 873 854 837 847)) (typed-worst-case . (1901 1895 1864 1887 1859 1878 1923 1877 1873 1892)) (untyped . (469 462 460 453 462 466 464 464 477 462))))))
          #s(commit-data "2019-03-15T22:13:10Z-0500_ce324be9f8b8ad8b88bc3a39e7b1de438b462c87"
                         #hasheq((acquire . #hasheq((typed . (865 866 869 854 848 852 864 847 847 838)) (typed-worst-case . (1889 1898 1923 1852 1876 1893 1917 1869 1863 1886)) (untyped . (486 457 467 466 469 462 470 476 464 467))))))
          #s(commit-data "2019-03-16T17:11:55Z-0400_a2d87c353eb3ae6431a91a2e924c2216756ff079"
                         #hasheq((acquire . #hasheq((typed . (840 860 847 860 858 868 840 865 841 840)) (typed-worst-case . (1876 1952 1893 1895 1905 1869 1963 1883 1882 1937)) (untyped . (470 469 471 473 482 468 480 462 465 470))))))
          #s(commit-data "2019-03-17T07:04:23Z-0500_ed2381ee595fa8ac06dded9aacaa4c34f5d73475"
                         #hasheq((acquire . #hasheq((typed . (857 865 871 851 842 854 852 845 872 859)) (typed-worst-case . (1902 1923 1884 1944 1892 1884 1922 1886 1901 1899)) (untyped . (467 461 470 462 473 466 468 466 474 475))))))
          #s(commit-data "2019-03-28T17:08:25Z-0500_7a9b1d065e168d882ac8800e3fed4340c940e3ae"
                         #hasheq((acquire . #hasheq((typed . (858 834 842 859 848 848 848 845 851 847)) (typed-worst-case . (1924 1940 1743 1927 1862 1883 1888 1918 1915 1902)) (untyped . (470 475 473 468 478 461 467 473 464 461))))))
          #s(commit-data "2019-03-28T17:08:25Z-0500_e1835074f5c44581cb9645f11f7ca8096e61a546"
                         #hasheq((acquire . #hasheq((typed . (853 863 847 845 861 848 862 851 869 846)) (typed-worst-case . (1907 1911 1910 1883 1934 1916 1886 1941 1867 1905)) (untyped . (472 470 455 470 469 454 478 470 461 465))))))
          #s(commit-data "2017-01-24T12:30:46Z-0600_9078bc9efb081231f80dce6ab1939d8ba3cf112f"
                         #hasheq((acquire . #hasheq((typed . (934)) (typed-worst-case . (5731)) (untyped . (426))))))
          #s(commit-data "2017-04-27T09:21:16Z-0500_70348f2b84459a5eb3e8feaa737e70028ece0747"
                         #hasheq((acquire . #hasheq((typed . (961)) (typed-worst-case . (5858)) (untyped . (439))))))
          #s(commit-data "2018-07-27T10:22:15Z-0500_00f2b69e22f7f5bf87c43fb2513fd8f2da269cef"
                         #hasheq((acquire . #hasheq((typed . (1066)) (typed-worst-case . (2971)) (untyped . (469))))))
          #s(commit-data "2018-10-26T14:51:55Z-0500_3475e86862a6fd5389ff5f22c456107c74fd05c5"
                         #hasheq((acquire . #hasheq((typed . (984)) (typed-worst-case . (2916)) (untyped . (462))))))
          #s(commit-data "2019-01-30T09:19:22Z-0600_5bf83b8ef26856bc473eaf74fc8ee4813e167f9e"
                         #hasheq((acquire . #hasheq((typed . (843)) (typed-worst-case . (1892)) (untyped . (462))))))
          #s(commit-data "2019-02-25T13:10:08Z-0700_84837f4330cef3df9271b778f2fbfba09d34fc3b"
                         #hasheq((acquire . #hasheq((typed . (854 843 857 875 852 861 873 854 837 847)) (typed-worst-case . (1901 1895 1864 1887 1859 1878 1923 1877 1873 1892)) (untyped . (469 462 460 453 462 466 464 464 477 462))))))
          #s(commit-data "2019-03-15T22:13:10Z-0500_ce324be9f8b8ad8b88bc3a39e7b1de438b462c87"
                         #hasheq((acquire . #hasheq((typed . (865 866 869 854 848 852 864 847 847 838)) (typed-worst-case . (1889 1898 1923 1852 1876 1893 1917 1869 1863 1886)) (untyped . (486 457 467 466 469 462 470 476 464 467))))))
          #s(commit-data "2019-03-16T17:11:55Z-0400_a2d87c353eb3ae6431a91a2e924c2216756ff079"
                         #hasheq((acquire . #hasheq((typed . (840 860 847 860 858 868 840 865 841 840)) (typed-worst-case . (1876 1952 1893 1895 1905 1869 1963 1883 1882 1937)) (untyped . (470 469 471 473 482 468 480 462 465 470))))))
          #s(commit-data "2019-03-17T07:04:23Z-0500_ed2381ee595fa8ac06dded9aacaa4c34f5d73475"
                         #hasheq((acquire . #hasheq((typed . (857 865 871 851 842 854 852 845 872 859)) (typed-worst-case . (1902 1923 1884 1944 1892 1884 1922 1886 1901 1899)) (untyped . (467 461 470 462 473 466 468 466 474 475))))))
          #s(commit-data "2019-03-28T17:08:25Z-0500_7a9b1d065e168d882ac8800e3fed4340c940e3ae"
                         #hasheq((acquire . #hasheq((typed . (858 834 842 859 848 848 848 845 851 847)) (typed-worst-case . (1924 1940 1743 1927 1862 1883 1888 1918 1915 1902)) (untyped . (470 475 473 468 478 461 467 473 464 461))))))
          #s(commit-data "2019-03-28T17:08:25Z-0500_e1835074f5c44581cb9645f11f7ca8096e61a546"
                         #hasheq((acquire . #hasheq((typed . (853 863 847 845 861 848 862 851 869 846)) (typed-worst-case . (1907 1911 1910 1883 1934 1916 1886 1941 1867 1905)) (untyped . (472 470 455 470 469 454 478 470 461 465)))))))))
  (save-pict "acquire.png"
             (car (make-machine-data-pict* aquire-data)))
)

