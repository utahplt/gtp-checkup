#lang racket/base

;; Tools for plotting parsed data.
;; See `data/` for raw data and `data/parse.rkt` for parsing tools.

(provide
  )

(require
  (only-in math/statistics mean)
  (only-in racket/string string-split)
  (only-in gregor ->posix parse-datetime)
  gtp-checkup/data/definition
  gtp-checkup/data/parse
  racket/runtime-path
  racket/generator
  pict-abbrevs
  pict
  plot/no-gui)

(module+ test (require rackunit racket/set))

;; =============================================================================

(define *wide-plot-width* (make-parameter 800))

(define (make-machine-data-pict* md)
  (define m-id (machine-data-id md))
  (define benchmark-name* (machine-data->benchmark-name* md))
  (define min-timeout (min* (machine-data->timeout* md)))
  (parameterize ([plot-x-ticks (date-ticks)]
                 [plot-width (*wide-plot-width*)]
                 [point-alpha 0.8])
    (for/list ([b-id (in-list benchmark-name*)])
      (define max-cpu-time (box 0))
      (plot-pict
        (for/list ((cfg (in-list configuration-name*)))
          (define point-color (configuration-name->color cfg))
          (define p# ;; Map result-kind to points
            (for/fold ((acc (make-immutable-hasheq '((ok . ()) (error . ()) (timeout . ())))))
                      ((cd (in-list (machine-data-commit* md))))
              (define ms (commit-id->ms (commit-data-id cd)))
              (define cfg# (hash-ref (commit-data-benchmark# cd) b-id))
              (define-values [k v]
                (let* ((r (hash-ref cfg# cfg))
                       (n (result->natural r)))
                  (when n
                    (set-box! max-cpu-time (max n (unbox max-cpu-time))))
                  (values (result->kind r) (or n min-timeout))))
              (hash-update acc
                           k
                           (lambda (p*)
                             (cons (vector ms v) p*)))))
          (for*/list (((r-kind p*) (in-hash p#))
                      #:unless (null? p*))
            (points p*
                    #:color point-color
                    #:fill-color point-color
                    #:sym (kind->symbol r-kind))))
        #:y-min 0
        #:y-max (min min-timeout (unbox max-cpu-time))
        #:width (plot-width)
        #:height (* 3/4 (plot-width))
        #:title (format "~a, ~a" b-id m-id)
        #:x-label "commit (by date, ascending)"
        #:y-label "runtime (s)"))))

(define (commit-id->ms cid)
  (define timestamp (car (string-split cid "_")))
  (->posix (parse-datetime timestamp "yyyy-MM-dd'T'HH:mm:ss'Z'xx")))

(module+ test
  (test-case "commit-id->ms"
    (check-equal? (commit-id->ms "2018-10-26T14:51:55Z-0500_3475e86862a6fd5389ff5f22c456107c74fd05c5")
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
  (let ((H (for/hasheq ((cfg (in-list configuration-name*))
                        (i (in-naturals 1)))
             (values cfg i))))
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

(define (result->natural x)
  (cond
    [(cpu-time*? x)
     (/ (mean x) 1000)]
    [(timeout? x)
     #f]
    [else
      0]))

;; =============================================================================

(module+ main
  (define-runtime-path data-dir "../data/")
  (define d (load-directory (build-path data-dir "nsa")))
  (save-pict "nsa.png"
             (apply vl-append 20 (make-machine-data-pict* d)))
)

