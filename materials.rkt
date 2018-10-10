#lang racket

(provide (struct-out material)
         disposable-material
         reusable-material

         apply-duplication-policy
         make-duplication-policy
         default-duplication-policy
         material:student

         duplication-system)

(require ts-racket)
(require (only-in pict pict?)
         (only-in 2htdp/image image?))

;Printing hints.  Or image differentiation...  Meta-data for materials...

(define printed 'printed)
(define disposable 'disposable)
(define reusable 'reusable)

(struct material (content types) #:transparent)

(define (disposable? m)
  (member disposable (material-types m)))

(define (disposable-material content)
  (material content
            (list printed
                  disposable)))

(define (reusable? m)
  (member reusable (material-types m)))

(define (reusable-material content)
  (material content
            (list printed
                  reusable)))


(define (material:student m s)
  (/ m s))

(define (apply-duplication-policy p quests)
  (map p quests))

(define (make-duplication-policy #:reusable   reusable-ratio
                                 #:disposable disposable-ratio
                                 #:raw-image  raw-image-ratio)
  
  (λ(quest)
    (λ(num-or-course)
      (define num-students (if (number? num-or-course)
                               num-or-course
                               (length (students num-or-course))))
      (flatten
       (map (duplicate-material num-students reusable-ratio disposable-ratio raw-image-ratio)
           quest)))))

(define (duplicate-material num-students reusable-ratio disposable-ratio raw-image-ratio)
  (λ(mat)
      (define ratio
        (cond [((or/c image? pict?)  mat) raw-image-ratio]
              [(reusable?   mat) reusable-ratio]
              [(disposable? mat) disposable-ratio]
              [else (begin
                      (displayln mat)
                      (error "What is this?"))]))
      (duplicate
       (* num-students ratio)
       mat)))

(define (duplicate n x)
  (map (thunk* x) (range n)))


(define default-duplication-policy
  (make-duplication-policy
   #:reusable   (material:student 1 3)
   #:disposable (material:student 1 1)
   #:raw-image  (material:student 1 1)))


(define-syntax-rule (duplication-system print-quest quests-expr policy)
  (begin
    (provide quests print-quest)

    (define (quests) quests-expr)

    (define/contract (print-quest q-number course-or-num)
      (-> number? (or/c course? number?) (listof (or/c material? image?)))

      (define quest-f (list-ref (quests) (sub1 q-number)))

      (define q (quest-f))
      
      ((policy q) course-or-num))))



