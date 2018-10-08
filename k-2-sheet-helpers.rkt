#lang racket

(provide bullets
         instructor-bg)

(require 2htdp/image)

(define title-font-size 18)

(define (bullet-pad i)
  (overlay i (rectangle (+ 10 (image-width i))
                        (+ 10 (image-height i))
                        'solid
                        'transparent)))

(define (->bullet s+i)
  (text (~a (second s+i) ". "
            (first s+i))
        title-font-size
        'black))

(define (bullets . ss)
  (apply (curry above/align "left")
         (map bullet-pad (map ->bullet (map list ss (range 1 (add1 (length ss))))))))

(define (instructor-bg i)
  (overlay
    i
    (rectangle (image-width i)
               (image-height i)
               'solid 'lightgray)))