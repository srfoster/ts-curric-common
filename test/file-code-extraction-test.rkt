#lang racket


(require "../main.rkt")

(require syntax/parse
         racket/runtime-path
         (prefix-in h: 2htdp/image)
         rackunit
         pict)

(require (only-in ts-racket code+hints hint random-dude))

(define-runtime-path starter-files "starter-files")

(define (test1)
  ;Define a syntax class for what you're looking for in the file
  (define-syntax-class player-sprite-def
    (pattern ((~datum define) (~datum player-sprite) expr)))

  ;Use that syntax class to find what you're looking for in some file
  ;  This will be your main code image
  (define extracted-snippet
    (extract-from-file (build-path starter-files "tsgd_runner_1.rkt")
                       player-sprite-def))

  ;Now typeset the main code image, while also finding image targets...
  (define-values (main hint-targets)
    (typeset-with-targets extracted-snippet
                          (list 3        change-this)
                          (list h:image? delete-this)))

  ;Produce a final code+hints image
  (code+hints main
              (list (first  hint-targets) (hint "Replace this"))
              (list (second hint-targets) (hint "And replace this"))))


;Dumb test.  Just see if it returns a pict
(check-equal? (pict? (test1)) #t)

(define (test2)
  ;Define a syntax class for what you're looking for in the file
  (define-syntax-class start-game-call
    (pattern ((~datum start-game) first:expr expr ... last:expr)))

  ;Use that syntax class to find what you're looking for in some file
  ;  This will be your main code image
  (define extracted-snippet
    (extract-from-file (build-path starter-files "tsgd_runner_1.rkt")
                       start-game-call))


  ;Now typeset the main code image, while also finding image targets...
  (define-values (main hint-targets)
    (typeset-with-targets extracted-snippet
                          (list 'bg-entity        delete-this)))


  ;Produce a final code+hints image
  (code+hints main
              (list (first  hint-targets) (hint "Delete the bg"))))



(check-equal? (pict? (test2)) #t)

;Example where we add something after something else
;   The trick?  Get the existing syntax.  Transform part of it to introduce a snipe target
;   Then snipe out the target with the new code

(define (test3)
  ;Define a syntax class for what you're looking for in the file
  (define-syntax-class start-game-call
    (pattern ((~datum start-game) first:expr expr ... last:expr)))

  ;Use that syntax class to find what you're looking for in some file
  ;  This will be your main code image
  (define extracted-snippet
    (extract-from-file (build-path starter-files "tsgd_runner_1.rkt")
                       start-game-call))

  
  (define extracted-snippet-transformed
    (syntax-case extracted-snippet (start-game)
      [(start-game first rest ... last)
       #`(start-game first
                     #,(datum->syntax #f 'SNIPE #'last)
                     rest
                     ...
                     last)]))


  (define-values (main hint-targets)
    (typeset-with-targets extracted-snippet-transformed
                          (list 'SNIPE
                                (replace-with (random-dude)))))


  (code+hints main
              (list (first hint-targets) (hint "Replace this.  Customize the values..."))))
  

(check-equal? (pict? (test3)) #t)

