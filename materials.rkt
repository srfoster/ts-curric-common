#lang racket

(provide (struct-out material)
         disposable-material
         reusable-material

         apply-duplication-policy
         make-duplication-policy
         default-duplication-policy
         material:student

         duplication-system

         save-out-materials)

(require ts-racket)
(require (prefix-in p: pict)
         2htdp/image)

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
      (map (duplicate-material num-students reusable-ratio disposable-ratio raw-image-ratio)
           quest))))

(define (duplicate-material num-students reusable-ratio disposable-ratio raw-image-ratio)
  (λ(mat)
      (define ratio
        (cond [((or/c image? p:pict?)  mat) raw-image-ratio]
              [(reusable?   mat) reusable-ratio]
              [(disposable? mat) disposable-ratio]
              [else (begin
                      (displayln mat)
                      (error "What is this?"))]))
      (duplicate
       (ceiling (* num-students ratio))
       mat)))

(define (duplicate n x)
  (map (thunk* x) (range n)))


(define default-duplication-policy
  (make-duplication-policy
   #:reusable   (material:student 1 3)
   #:disposable (material:student 1 1)
   #:raw-image  (material:student 1 1)))


(define (->thumb i-or-m)
  (define i (if (material? i-or-m)
                (material-content i-or-m)
                i-or-m))
  (scale-to-fit
   (any->image i)
   100))

(define (summarize materials duplicated-materials)
  (map list
       (map ->thumb materials)
       (map length duplicated-materials)))

(define (render-summary-line pair)
  (frame 
   (beside (text (~a "x" (second pair))
                 20
                 'black)
           (first pair))))

(define (render-summary summary)
  (above/align "left"
   (text "Duplication Summary" 24 'black)
   (apply (curry above/align "left")
          (map render-summary-line summary))))


(define (make-summary-sheet materials duplicated-materials)
  (render-summary
   (summarize materials duplicated-materials)))

(define-syntax-rule (duplication-system print-quest quests-expr policy)
  (begin
    (provide quests print-quest)

    (define (quests) quests-expr)

    (define/contract (print-quest q-number course-or-num)
      (-> number? (or/c course? number?) (listof (or/c material? image?)))

      (define quest-f (list-ref (quests) (sub1 q-number)))

      (define q (quest-f))
      
      (define ret ((policy q) course-or-num))

      (define summary-sheet (make-summary-sheet q ret))
      
      (append (flatten ret)
              (list summary-sheet))
      )))


;PRINTING

(require (only-in racket/gui get-directory))

(define (save-out-materials prefix materials)
  (save-all (get-directory)
            prefix
            (map (λ(m)
                   (if (material? m)
                       (material-content m)
                       m))
                 materials)))



(define (save-all folder prefix is)
  
  (for ([n (range (length is))])
    (define i (list-ref is n))
    (define name (build-path folder (~a prefix "-" n ".png")))
    (define save (if (image? i)
                     (curryr save-image name)
                     (curryr save-pict name 'png)))
    (save i))

  (system (~a "open " (path->string folder))))

 
(define (save-pict the-pict name kind)
  (define bm (p:pict->bitmap the-pict))
  (send bm save-file name kind))

