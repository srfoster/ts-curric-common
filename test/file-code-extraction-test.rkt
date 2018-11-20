#lang racket


(require "../main.rkt")

(require syntax/parse
         racket/runtime-path
         (prefix-in h: 2htdp/image)
         rackunit
         pict)

(require (only-in ts-racket code+hints hint random-dude))

(define-runtime-path starter-files "starter-files")


;A syntax class found in the file  "tsgd_runner_1.rkt"
(define-syntax-class player-sprite-def #:datum-literals (define player-sprite sheet->sprite)
  (pattern (define player-sprite
             (sheet->sprite image:expr
                            columns-kw:keyword columns:number))))


(define-syntax-class enemy-entity-def #:datum-literals (enemy-entity sprite->entity sheet->sprite)
  (pattern
   (define (enemy-entity)
     (sprite->entity (sheet->sprite image:expr
                                    image-options ...)
                     name-kw:keyword name
                     position-kw:keyword position
                     components:keyword first-component rest ...))))



(define (test1)


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




(define (test4)

  ;Grab a snippet with an image in it
  (define extracted-snippet
    (extract-from-file (build-path starter-files "tsgd_runner_1.rkt")
                       enemy-entity-def))

  

  ;Typeset the un-transformed snippet
  (define-values (f hint-targets2)
    (typeset-with-targets extracted-snippet))


  ;Arbitrary transformation
  (define extracted-snippet-transformed
    (syntax-parse extracted-snippet
      [p:enemy-entity-def
       #`(define (player-entity)
           (sprite->entity (sheet->sprite p.image
                                          p.image-options ...)
                           p.name-kw p.name
                           p.position-kw p.position
                           p.components p.first-component p.rest ...
                           #,(datum->syntax #f 'SNIPE #'p.first-component)))]
      [other (error "Error transforming snippet")]))


  ;Typeset the transformed snippet
  (define-values (s hint-targets1)
    (typeset-with-targets extracted-snippet-transformed
                          (list 'SNIPE
                                (replace-with 'SNIPED))))



  ;Make sure the embedded image stays...
  (list f s))


(let ([t (test4)])
  (check-equal? (<= (pict-height (first t))
                    (pict-height (second t)))
                #t
                "Embedded images should not get removed from typesetted snippets."))











(define (test5)

  ;Grab a snippet with an image in it
  (define extracted-snippet
    (extract-from-file (build-path starter-files "tsgd_enemy.rkt")
                       enemy-entity-def))

  
  ;Typeset the transformed snippet
  (define-values (main hint-targets)
    (typeset-with-targets extracted-snippet))



  ;Make sure the embedded image stays...
  main)


(check-equal? (pict? (test5)) #t)






