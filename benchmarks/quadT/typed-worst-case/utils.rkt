#lang typed/racket/base #:no-optimize

(provide
  hyphenate-quad
  quad-map
  split-quad
  add-vert-positions
  compute-line-height
  quad-attr-set
  group-quad-attr-set
  quad-attr-set*
  group-quad-attr-set*
  quad-attr-remove*
  group-quad-attr-remove*
  merge-attrs
  flatten-quad
  flatten-quadtree
  attr-change
  split-last
  attr-delete ;; for wrap.rkt
  join-quads
)

;; -----------------------------------------------------------------------------

(require
  "../base/core-types.rkt"
  "../base/quad-types.rkt"
  (only-in racket/list append-map empty? empty split-at-right first splitf-at)
  (only-in racket/string string-append*)
  (only-in math/flonum fl+)
)
(require/typed "hyphenate.rkt"
  (hyphenate (->* (String)
                  ((U Char String)
                   #:exceptions (Listof String)
                   #:min-length Index
                   #:min-left-length Index
                   #:min-right-length Index
                   #:omit-word (-> String Boolean)
                   #:omit-string (-> String Boolean))
                  String)))
(require/typed "measure.rkt"
  [round-float (-> Float Float)])
(require/typed "quads.rkt"
  [word (->* ((U QuadAttrs HashableList)) () #:rest QuadListItem WordQuad)]
  [quad-name (-> Quad QuadName)]
  [quad-attrs (-> Quad QuadAttrs)]
  [make-quadattrs (-> (Listof Any) QuadAttrs)]
  [quad-list
   (Quad -> QuadList)]
  [group-quad-list
   (GroupQuad -> GroupQuadList)]
  [box (->* ((U QuadAttrs HashableList))()  #:rest QuadListItem BoxQuad)]
  [whitespace/nbsp? (-> Any Boolean)]
  [quad-attr-ref (->* ((U Quad QuadAttrs) QuadAttrKey) (QuadAttrValue) QuadAttrValue)]
)
(require/typed "world.rkt"
  [world:font-size-key QuadAttrKey]
  [world:font-size-default (Parameterof Positive-Float)]
  [world:font-name-key QuadAttrKey]
  [world:font-name-default (Parameterof Font-Name)]
  [world:font-weight-key QuadAttrKey]
  [world:font-weight-default (Parameterof Font-Weight)]
  [world:font-style-key QuadAttrKey]
  [world:font-style-default (Parameterof Font-Style)]
  [world:mergeable-quad-types (Listof Symbol)]
  [world:leading-key QuadAttrKey]
  [world:leading-key-default (Parameterof Float)]
  [world:height-key Symbol]
  [world:split-quad-key Symbol]
  [world:x-position-key Symbol]
  [world:y-position-key Symbol])

;; =============================================================================

(: quad-map ((QuadListItem -> QuadListItem) Quad -> Quad))
(define (quad-map proc q)
  (quad (quad-name q) (quad-attrs q) (map proc (quad-list q))))

;; push together multiple attr sources into one list of pairs.
;; mostly a helper function for the two attr functions below.
;; does not resolve duplicates (see merge-attrs for that)
(: join-attrs ((Listof JoinableType) -> QuadAttrs))
(define (join-attrs quads-or-attrs-or-lists)
  (append-map (λ([x : JoinableType])
                (cond
                  [(quad? x) (quad-attrs x)]
                  [(quad-attrs? x) x]
                  [else (make-quadattrs x)])) quads-or-attrs-or-lists))


;; merge uses join-attrs to concatenate attributes,
;; but then resolves duplicates, with later ones overriding earlier.
(: merge-attrs (JoinableType * -> QuadAttrs))
(define (merge-attrs . quads-or-attrs-or-lists)
  (define all-attrs (join-attrs quads-or-attrs-or-lists))
  (hash->list (make-hash all-attrs)))

;;; flatten merges attributes, but applies special logic suitable to flattening
;;; for instance, resolving x and y coordinates.
(define-type QuadAttrFloatPair (Pairof QuadAttrKey Float))

(: flatten-attrs (JoinableType * -> QuadAttrs))
(define (flatten-attrs . joinable-items)
  (define all-attrs (join-attrs joinable-items))
  (define-values (x-attrs y-attrs other-attrs-reversed)
    (for/fold ([xas : (Listof QuadAttrFloatPair) null]
               [yas : (Listof QuadAttrFloatPair) null]
               [oas : (Listof QuadAttr) null])
              ([attr (in-list all-attrs)])
      (cond
        [(and (equal? (car attr) world:x-position-key) (flonum? (cdr attr))) (values (cons attr xas) yas oas)]
        [(and (equal? (car attr) world:y-position-key) (flonum? (cdr attr))) (values xas (cons attr yas) oas)]
        [else (values xas yas (cons attr oas))])))
  (: make-cartesian-attr (QuadAttrKey (Listof QuadAttrFloatPair) -> (Listof QuadAttrFloatPair)))
  (define (make-cartesian-attr key attrs)
    (if (empty? attrs)
        empty
        (list (cons (ann key QuadAttrKey) (foldl fl+ 0.0 ((inst map Float QuadAttrFloatPair) cdr attrs))))))
  (define x-attr (make-cartesian-attr world:x-position-key x-attrs))
  (define y-attr (make-cartesian-attr world:y-position-key y-attrs))
  ;; use hash to resolve duplicate entries by giving priority to later ones
  ;; then stuff x & y at the front (they will not have duplicates because they were already resolved)
  (append x-attr y-attr (hash->list ((inst make-hash QuadAttrKey QuadAttrValue) (reverse other-attrs-reversed)))))

;; ordinary flatten won't work because a quad is a bare list,
;; and flatten will go too far.
;; this version adds a check for quadness to the flattener.
(: flatten-quadtree ((Treeof Quad) -> (Listof Quad)))
(define (flatten-quadtree quad-tree)
  (let loop ([sexp quad-tree][acc : (Listof Quad) null])
    (cond [(null? sexp) acc]
          [(quad? sexp) (cons sexp acc)]
          [else (loop (car sexp) (loop (cdr sexp) acc))])))

;; starting with a single nested quad,
;; pushes attributes down from parent quads to children,
;; resulting in a flat list of quads.
(: flatten-quad (Quad -> (Listof Quad)))
(define (flatten-quad q)
  (flatten-quadtree
   (let loop : (Treeof Quad)
     ([x : QuadListItem q][parent : Quad (quad 'null '() '())])
     (cond
       [(quad? x)
        (let ([x-with-parent-attrs (quad (quad-name x)
                                         (flatten-attrs parent x) ; child positioned last so it overrides parent attributes
                                         (quad-list x))])

          (if (empty? (quad-list x))
              x-with-parent-attrs ; no subelements, so stop here
              ((inst map (Treeof Quad) QuadListItem) (λ(xi) (loop xi x-with-parent-attrs)) (quad-list x))))] ; replace quad with its elements
       [else ;; it's a string
        (quad (quad-name parent) (quad-attrs parent) (list x))]))))

;; flatten quad as above,
;; then dissolve it into individual character quads while copying attributes
;; input is often large, so macro allows us to avoid allocation
(: split-quad (Quad -> (Listof Quad)))
(define (split-quad q)
  (: do-explode ((QuadListItem) (Quad) . ->* . (Treeof Quad)))
  (define (do-explode x [parent (box '())])
    (cond
      [(quad? x)
       (if (empty? (quad-list x))
           x ; no subelements, so stop here
           ((inst map (Treeof Quad) QuadListItem) (λ(xi) (do-explode xi x)) (quad-list x)))] ; replace quad with its elements, exploded
      [else ;; it's a string
       ((inst map (Treeof Quad) QuadListItem) (λ(xc) (quad world:split-quad-key (quad-attrs parent) (list xc))) (regexp-match* #px"." x))]))
  (flatten-quadtree (map do-explode (flatten-quad q))))



;; merge chars into words (and boxes), leave the rest
;; if two quads are mergeable types, and have the same attributes,
;; they get merged.
;; input is often large, so macro allows us to avoid allocation
(: join-quads ((Listof Quad) -> (Listof Quad)))
(define (join-quads qs-in)
  (let ([make-matcher (λ ([base-q : Quad])
                        (λ([q : Quad])
                          (and (member (quad-name q) world:mergeable-quad-types)
                               (not (whitespace/nbsp? q))
                               ;; if key doesn't exist, it is compared against the default value.
                               ;; this way, a nonexistent value will test true against a default value.
                               (andmap (λ([key : QuadAttrKey] [default : QuadAttrValue]) (equal? (quad-attr-ref base-q key default) (quad-attr-ref q key default)))
                                       (ann (list world:font-name-key
                                                  world:font-size-key
                                                  world:font-weight-key
                                                  world:font-style-key) (Listof QuadAttrKey))
                                       (ann (list (world:font-name-default)
                                                  (world:font-size-default)
                                                  (world:font-weight-default)
                                                  (world:font-style-default)) (Listof QuadAttrValue))))))])
    (let loop ([qs : (Listof Quad) qs-in][acc : (Listof Quad) null])
      (if (null? qs)
          (reverse acc)
          (let* ([base-q (first qs)]
                 [mergeable-and-matches-base? (make-matcher base-q)]) ; make a new predicate function for this quad
            (cond
              [(mergeable-and-matches-base? base-q)
               ;; take as many quads that match, using the predicate function
               (define-values (matching-qs other-qs) (splitf-at (cdr qs) mergeable-and-matches-base?))
               (define new-word-strings (append-map quad-list (cons base-q matching-qs)))
               (define new-word
                 (if (andmap string? new-word-strings)
                     (word (quad-attrs base-q) (string-append* new-word-strings))
                     (error 'join-quads "expected string")))
               (loop other-qs (cons new-word acc))]
              ;; otherwise move on to the next in line
              [else (loop (cdr qs) (cons base-q acc))]))))))

;; these helper functions isolate the generic functionality.
;; problem with quad-attr-set and other Quad->Quad functions
;; is that they strip out type.
;; whereas these "surgical" alternatives can be used when preserving type is essential
(: attr-change (QuadAttrs HashableList -> QuadAttrs))
(define (attr-change qas kvs)
  (merge-attrs qas kvs))

(: attr-delete (QuadAttrs QuadAttrKey * -> QuadAttrs))
(define (attr-delete qas . ks)
  (filter (λ([qa : QuadAttr]) (not (ormap (λ(k) (equal? (car qa) k)) ks))) qas))


;; functionally update a quad attr. Similar to hash-set
(: quad-attr-set
   (Quad QuadAttrKey QuadAttrValue -> Quad))
(define (quad-attr-set q k v)
  (quad-attr-set* q (list k v)))
(: group-quad-attr-set
   (GroupQuad QuadAttrKey QuadAttrValue -> GroupQuad))
(define (group-quad-attr-set q k v)
  (group-quad-attr-set* q (list k v)))


;; functionally update multiple quad attrs. Similar to hash-set*
(: quad-attr-set*
   (Quad HashableList -> Quad))
(define (quad-attr-set* q kvs)
  (quad (quad-name q) (attr-change (quad-attrs q) kvs) (quad-list q)))
(: group-quad-attr-set*
   (GroupQuad HashableList -> GroupQuad))
(define (group-quad-attr-set* q kvs)
  (quad
    (quad-name q)
    (attr-change (quad-attrs q) kvs)
    (group-quad-list q)))

;; functionally remove multiple quad attrs. Similar to hash-remove*
(: quad-attr-remove*
   (Quad QuadAttrKey * -> Quad))
(define (quad-attr-remove* q . ks)
  (if (not (empty? (quad-attrs q)))
      ;; test all ks as a set so that iteration through attrs only happens once
      (quad (quad-name q) (apply attr-delete (quad-attrs q) ks) (quad-list q))
      q))
(: group-quad-attr-remove*
   (GroupQuad QuadAttrKey * -> GroupQuad))
(define (group-quad-attr-remove* q . ks)
  (if (not (empty? (quad-attrs q)))
      ;; test all ks as a set so that iteration through attrs only happens once
      (quad (quad-name q) (apply attr-delete (quad-attrs q) ks) (group-quad-list q))
      q))

;; todo: how to guarantee line has leading key?
(: compute-line-height (Quad -> Quad))
(define (compute-line-height line)
  (quad-attr-set line world:height-key
    (quad-attr-ref line world:leading-key (world:leading-key-default))))

(: quad-height (-> (U GroupQuad Quad) Float))
(define (quad-height q)
  (assert (quad-attr-ref q world:height-key 0.0) flonum?))

;; use heights to compute vertical positions
(: add-vert-positions (GroupQuad -> GroupQuad))
(define (add-vert-positions starting-quad)
  (define-values (new-quads final-height)
    (for/fold ([new-quads : (Listof (U GroupQuad Quad)) empty][height-so-far : Float 0.0])
              ([q (in-list (group-quad-list starting-quad))])
      (values (cons (quad-attr-set q world:y-position-key height-so-far) new-quads)
              (round-float (+ height-so-far (quad-height q))))))
  (quad (quad-name starting-quad) (quad-attrs starting-quad) (reverse new-quads)))

;; recursively hyphenate strings in a quad
(: hyphenate-quad (QuadListItem -> QuadListItem))
(define (hyphenate-quad x)
  (cond
    [(quad? x) (quad-map hyphenate-quad x)]
    [(string? x) (hyphenate x
                            #:min-length 6
                            #:min-left-length 3
                            #:min-right-length 3)]
    [else x]))

;; just because it comes up a lot
(: split-last (All (A) ((Listof A) -> (values (Listof A) A))))
(define (split-last xs)
  (let-values ([(first-list last-list) ((inst split-at-right A) xs 1)])
    (values first-list (car last-list))))

