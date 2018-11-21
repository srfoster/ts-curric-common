#lang racket

(provide define-classroom-flow
         define-CODE-resource
         define-ABSORB-resource
         activity
         choose-any
         define-quest-data)

(require (for-syntax racket))

(define-syntax-rule (define-classroom-flow id settings cards ...)
  (begin
    (require "./common.rkt")
    (require ts-curric-common)
    (define (temp)
      (list
       cards
       ...))

    (define s settings)

    (define (id)
      (map shrink (make-picts "red" "QX-" (temp) s)))

    (module+ test
      (analyze-activities (temp) s))))



(define-syntax (define-CODE-resource stx)
  (syntax-case stx (define-CODE-resource)
    [(_ 
      id
      #:in-file f
      #:code-change the-change
      #:description h)
     (with-syntax ([pkg-name (findf
                              (curryr string-prefix? "ts-curric-")
                              (map ~a (explode-path (syntax-source stx))))])
       #`(begin
           (require "./common.rkt")
           (require ts-curric-common)
           (require (only-in ts-racket code+hints hint))
     
           (define (the-code-image)

             (define-values (main hint-targets)
               (try-smw-and-then f
                                 the-change))
  
             (code+hints main
                         (list (first hint-targets) h)))


           (provide id)
           (define id
             (define-launcher-function-func
               pkg-name
               'id
               the-code-image))

           ))]))

(define-syntax (define-ABSORB-resource stx)
  (syntax-case stx (define-ABSORB-resource)
    [(_ 
      id
      #:learning-resource r)
     (with-syntax ([pkg-name (findf
                              (curryr string-prefix? "ts-curric-")
                              (map ~a (explode-path (syntax-source stx))))])
       #`(begin
           (require "./common.rkt")
           (require ts-curric-common)
           (require (only-in ts-racket code+hints hint))
     
           (define (the-code-image)
             r)


           (provide id)
           (define id
             (define-launcher-function-func
               pkg-name
               'id
               the-code-image))

           ))]
    ))


(require (only-in pict pict? ghost circle)
         "./half-sheets.rkt"
         "./materials.rkt")
(define (activity #:award a
                  #:title t
                  #:show-teacher (g #f)
                  
                  #:resource (r (ghost (circle 1)))
                  #:instructions s . ss)
  (with-award a
    (activity-instructions t
                         '()
                         (append
                          (map instruction-basic (flatten (cons s ss)))
                          (list                        
                           (if g
                               (instruction-goal g)
                               '())))
                         (if (pict? r)
                             r
                             (launcher-img r)))))



(define (choose-any . as)
  (choose "any" as))


(define-syntax (define-quest-data stx)
  (syntax-case stx ()
    [(_ id:identifier #:title title:string #:at-minimum-student-can things ...)
     #'(displayln "hi")])
  )


