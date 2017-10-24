#lang racket/base

(provide quick-sample)

;; -----------------------------------------------------------------------------

(require
  require-typed-check
  racket/file
  (only-in racket/include include)
(only-in "quads.rkt"
  page-break
  column-break
  word
  box
  block
 block-break))

;; =============================================================================

(define (quick-sample)
  (block '(measure 240.0 font "Times New Roman" leading 16.0 vmeasure 300.0 size 13.5 x-align justify x-align-last-line left)
         (box '(width 15.0))
         (block '()
                (block '(weight bold) "Hot " (word '(size 22.0) "D") "ang, My Fellow Americans.")
                " This "
                (block '(no-break #t) "is some truly")
                " nonsense generated from my typesetting system, which is called Quad. I’m writing this in a source file in DrRacket. When I click [Run], a PDF pops out. Not bad\u200a—\u200aand no LaTeX needed. Quad, however, does use the fancy linebreaking algorithm developed for TeX. (It also includes a faster linebreaking algorithm for when speed is more important than quality.) Of course, it can also handle "
                (block '(font "Courier") "different fonts,")
                (block '(style italic) " styles, ")
                (word '(size 14.0 weight bold) "and sizes-"))))
