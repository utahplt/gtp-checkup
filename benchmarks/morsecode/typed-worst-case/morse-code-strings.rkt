#lang typed/racket/base #:no-optimize

;; Copyright 2013 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0

;; this file contains functions to convert text into sounds

;;bg
;; Original file would make a SOUND from the sequence of dots and dashes.
;; We just make the . and -

(provide string->morse)

(require/typed "morse-code-table.rkt"
  [char-table (HashTable Char String)])

;; map a character to a dit-dah string
(: char->dit-dah-string (-> Char String))
(define (char->dit-dah-string letter)
  (define res (hash-ref char-table (char-downcase letter) #f))
  (if (eq? #f res)
      (raise-argument-error 'letter-map "character in map"
                              0 letter)
      res))

(: string->morse (-> String String))
(define (string->morse str)
  (define morse-list 
    (for/list : (Listof String) ([c : Char str])
      (char->dit-dah-string c)))
  (apply string-append morse-list))
