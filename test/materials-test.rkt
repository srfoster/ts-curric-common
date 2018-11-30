#lang racket

(require rackunit
         pict
         "../materials.rkt")

(define (quest1)
  (list
   (colorize (rectangle 300 250) "red")
   (colorize (rectangle 300 250) "green")
   (colorize (rectangle 300 250) "blue")

   (colorize (rectangle 650 800) "gray")))

(define (quest2)
  (list

   ))

(define (quests)
  (list quest1
        quest2))

(duplication-system printable-quest
                    
 (quests)

 (make-duplication-policy
  #:reusable   (material:student 1 1)
  #:disposable (material:student 1 1)
  #:raw-image  (material:student 1 1)))


(check-equal? (length (printable-quest 1 1))
              2
              "After printing converstion, quest 1 should only have 2 images to print")






