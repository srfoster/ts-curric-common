#lang racket

(require (except-in 2htdp/image frame)
         (prefix-in p: pict/code)
         (prefix-in p: pict)
         ts-racket
         game-engine-demos-common)


(define bubble-color
  'black)

(struct card (question answers correct-answer))

(define letters
  '(A B C D E F G))

(define (number->letter n)
  (list-ref letters n))

(define (letter->number n)
  (index-of letters n))

(define (answer->image a)
  (text (~a (number->letter (first a))
            ": "
            (second a))
        24
        'gray))

(define (render c)
  (define i (above/align "left"
                         (if (image? (card-question c))
                             (card-question c)
                             (text (card-question c) 24 'black))
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
   (beside slanty (rectangle 200 1 'solid 'transparent))))

(define (talking-render c)
  (above/align "left"
               (bubble-render c)
               (scale 2
                      (crop 0 0
                            32 48
                            (sith-character)))
               #;(scale 0.5
                        (random-dude))))
 
(define card1
  (card "What's your favorite color?"
        (list "Cheese"
              "Pizza"
              "Ham"
              "Soda")
        0))

(define card2
  (card "What's your favorite food?"
        (list "Red"
              "Green"
              "Yellow"
              "Blue")
        1))

(define card3
  (card (scale 2 (p:pict->bitmap (p:code (circle 40 "solid" "red"))))
        (list "Red"
              "Green"
              "Yellow"
              "Blue")
        1))


(define current-card-index 0)

(define cards
  (list card3 card1 card2))

(define (current-card)
  (list-ref cards current-card-index))

(define (next-card)
  (define c (current-card))

  (talking-render c))


(define (answer letter)
  (if (= (letter->number letter)
         (card-correct-answer (current-card)))
      (begin
        (set! current-card-index (add1 current-card-index))
        (text "RIGHT" 50 'green))
      (text "WRONG" 50 'red)))





