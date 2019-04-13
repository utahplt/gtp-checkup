#lang racket/base

;; Tools for plotting parsed data.
;; See `data/` for raw data and `data/parse.rkt` for parsing tools.

(provide
  )

(require
  gtp-checkup/data/definition
  gtp-checkup/data/parse
  plot/no-gui)

(module+ test (require rackunit))

;; =============================================================================

(define *wide-plot-width* (make-parameter 800))

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
              ((k _) (in-hash (commit-data-benchmark# cd))))
    (list-insert acc k symbol<=?)))

(define (symbol<=? a b)
  (or (eq? a b)
      (symbol<? a b)))

(define (make-machine-data-pict* md)
  (define m-id (machine-data-id md))
  (define benchmark-name* (machine-data->benchmark-name* md))
  (define min-timeout TODO)
  (define max-cpu-time TODO)
  (parameterize ([plot-x-ticks date-ticks]
                 [plot-width *wide-plot-width*]
                 [point-alpha 0.8])
    (for/list ([b-id (in-list benchmark-name*)])
      (plot-pict
        (for/list ((cfg (in-list configuration-name*)))
          (define point-color (configuration-name->color cfg))
          (define p# ;; Map result-kind to points
            (for/fold ((acc (make-immutable-hasheq '((ok . ()) (error . ()) (timeout . ())))))
                      ((cd (in-list (machine-data-commit* md))))
              (define c-id (commit-data-id cd))
              (define cfg# (hash-ref (commit-data-benchmark# cd) b-id))
              (define-values [k v]
                (let ((r (hash-ref cfg# cfg)))
                  (values (result->kind r) (result->natural r))))
              (hash-update acc
                           k
                           (lambda (p*)
                             (cons (cons c-id v) p*)))))
          (for*/list (((r-kind p*) (in-hash p#)))
            (points (list p*)
                    #:color point-color
                    #:fill-color point-color
                    #:sym (kind->symbol r-kind))))
        #:x-min 0
        #:x-max (min min-timeout max-cpu-time)
        #:width (plot-width)
        #:height (* 3/4 (plot-width))
        #:title (format "~a, ~a" b-id m-id)
        #:x-label "commit (by date, ascending)"
        #:y-label "runtime (s)"))))

(define TODO 0)

(define (kind->symbol x)
  TODO)

(define (configuration-name->color x)
  TODO)

(define (result->kind x)
  TODO)

(define (result->natural x)
  TODO)


;; =============================================================================

(module+ main
  (require pict racket/runtime-path pict-abbrevs)
  (define-runtime-path data-dir "../data/")
  (define d (load-directory (build-path data-dir "nsa")))
  (save-pict "nsa.png"
             (apply vl-append 20 (make-machine-data-pict* d)))
)

