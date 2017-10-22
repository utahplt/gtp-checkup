#lang typed/racket

(require
         "data-adaptor.rkt")
(require/typed "const.rkt"
                     [WORLD (-> World)])
(require/typed "motion.rkt"
                     [reset!           (-> Void)]
                     [world->world     (World . -> . World)])
(require/typed "handlers.rkt"
                     [handle-key (World String . -> . World)]
                     [game-over? (World . -> . Boolean)])

(: replay : World (Listof Any) -> Void)
(define (replay w0 hist)
  (reset!)
  (let loop ((w : World w0)
             (h : (Listof Any) hist))
    (if (empty? h)
        w
        (let ()
          (loop
           (match (car h)
             [`(on-key ,(? string? ke))
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (cdr h)))))
  (void))

(define SMALL_TEST "snake-hist-small.rktd")

(: main (-> String Void))
(define (main filename)
  (define w0 (WORLD))
  (define raw-hist (with-input-from-file filename read))
  (cond [(list? raw-hist)
         (define hist (reverse raw-hist))
         (for ([i (in-range 100)])
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main SMALL_TEST))
