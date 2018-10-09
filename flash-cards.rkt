#lang racket

(require 2htdp/image
         ts-racket)


(define bubble-color
  'black)

(struct card (question answers correct-answer))

(define (number->letter n)
  (list-ref '(A B C D E F G) n))

(define (answer->image a)
  (text (~a (number->letter (first a))
            ": "
            (second a))
        24
        'gray))

(define (render c)
  (define i (above/align "left"
                         (text (card-question c) 24 'black)
                         (apply (curry above/align "left")
                                (map answer->image (map list
                                                        (range (length (card-answers c)))
                                                        (card-answers c))))))
  (overlay
   i
   (rectangle (+ 50 (image-width i))
              (+ 50 (image-height i))
              'solid 'white)))

(define (bubble-render c)
  (define slanty  (triangle/sss 80 40 60 "solid" bubble-color))
  (define i (render c))
  (above
   (overlay
    i
    (rectangle (+ 10 (image-width i))
               (+ 10 (image-height i))
               'solid
               bubble-color))
   (beside slanty (rectangle 100 1 'solid 'transparent))))

(define (talking-render c)
  (above/align "left"
   (bubble-render c)
   (scale 0.5 (random-dude))))
 
(define card1
  (card "What's your favorite color?"
        (list "Cheese"
              "Pizza"
              "Ham"
              "Soda")
        0))

(talking-render card1)
