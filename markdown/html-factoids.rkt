#lang racket/base

(provide (all-defined-out))

(define (void-element? x)
  ;; http://www.w3.org/TR/html-markup/syntax.html#void-element
  (memq (car x)
        '(area base br col command embed hr img input keygen link
               meta param source track wbr)))

(define block-tags '(address blockquote body center dir div dl fieldset
                             form h1 h2 h3 h4 h5 h6 head hr html isindex
                             menu noframes noscript ol p pre table ul dd
                             dt frameset li tbody td tfoot th thead tr
                             script style))

(define inline-tags '(a abbr acronym b basefont bdo big br cite code dfn
                        em font i img input kbd label legend map q s samp
                        select small span strike strong sub sup textarea
                        tt u var))

(define either-tags '(applet button del iframe ins map area object))

(define (block-tag? tag)
  (or (memq tag block-tags) (memq tag either-tags)))

(define (inline-tag? tag)
  (or (memq tag inline-tags) (memq tag either-tags)))
