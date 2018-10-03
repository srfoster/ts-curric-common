#lang slideshow

(provide sci-fi-block-textures)

(require simple-qr)
(require "half-sheets.rkt")

;(define BG-BROWN (scale (bitmap "bg-brown.png") 0.80))
;(define BG-BLUE (scale (bitmap "bg-blue.png") 0.80))

;(define YODA-AVATAR   (scale (bitmap "yoda-avatar.png") 0.80))
;(define OBIWAN-AVATAR (scale (bitmap "obiwan-avatar.png") 0.80))


;Sci-Fi Blocks Textures

(define (custom-texture-reward element);;rewards
  (with-award -5 (activity-instructions "StarWars Block"
                                       '()
                                       (list
                                        (instruction-image (++ "minecraft/" (first element) "-" (number->string (second element)) ".png") 200 200 "")
                                        (instruction-basic "Scan your QR to download your custom texture."))
                                       (++ "https://secure.thoughtstem.com/tss/mt/" (first element) (number->string (second element)) ".png"))))

(define list-colors (list "red" "blue" "green" "orange" "purple" "white"))

(define list-numbers (range 1 17))

(define combined-lists (cartesian-product list-colors list-numbers))

(define sci-fi-block-textures
  (map custom-texture-reward combined-lists))


;(map slide (make-picts rewards BG-BLUE YODA-AVATAR))