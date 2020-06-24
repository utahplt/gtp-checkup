#lang typed/racket/base

(require/typed "image.rkt"
  (#:struct image ((impl : Any)))
  (empty-scene (-> Real Real Image))
  (place-image (-> Image Real Real Image Image))
  (circle (-> Real String String Image))
)
(define-type Image image)

(provide
  Image
  empty-scene
  place-image
  circle
)
