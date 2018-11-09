#lang racket

(provide (struct-out material)
         disposable-material
         reusable-material

         apply-duplication-policy
         make-duplication-policy
         default-duplication-policy
         material:student

         duplication-system

         save-out-materials
         save-any

         launch
         student-display
         
         (struct-out defined-image)
         define-quests
         require/provide-common

         launcher-img
         
         define-image-file
         define-webpage
         define-racket-file
         define-launcher-list
         define-launcher-function

         (struct-out defined-launcher-function)
         (struct-out defined-racket-file)

         how-to-use-launcher-card
         )

(require "./half-sheets.rkt")
(require ts-racket)
(require net/sendurl)
(require framework)
(require (only-in racket/gui editor-snip%))

(require (prefix-in p: pict)
         (prefix-in p: pict/code)
         2htdp/image)

(require (for-syntax racket))

(struct defined-launcher (package-name id) #:transparent)

(struct defined-image defined-launcher (image))

(struct defined-webpage defined-launcher (url) #:transparent)

(struct defined-racket-file defined-launcher (path) #:transparent)

(struct defined-launcher-list defined-launcher (launchers) #:transparent)

(struct defined-launcher-function defined-launcher (f) #:transparent)

(define launchable? defined-launcher?)


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


;I Like the idea of this, but it's currenty breaking above/align when there
; arent enough images. Fix before using...
(define (render-summary summary)
  (define l (map render-summary-line summary))

  
  (above/align "left"
               (text "Duplication Summary" 24 'black)
               (apply (curry above/align "left")
                      l))   )


(define (make-summary-sheet materials duplicated-materials)
  (render-summary
   (summarize materials duplicated-materials)))


(define (quest-card? c)
  (define i (if (material? c) (material-content c) c))
  
  (and (p:pict? i)
       (> 600 (p:pict-width i))))



(define (handle-card c)
  (define i (if (material? c) (material-content c) c))
  
  (p:frame #:line-width 2
           (p:inset i 20)))


(define (handle-cards materials)
  
  (define cards     (filter quest-card? materials))
  (define not-cards (filter (not/c quest-card?) materials))

  (append (if (empty? cards)
              '()
              (cards->pages (map handle-card cards)))
          not-cards))

(define-syntax-rule (duplication-system print-quest quests-expr policy)
  (begin
    (provide quests print-quest)

    (define (quests) quests-expr)

    (define/contract (print-quest q-number course-or-num)
      (-> number? (or/c course? number?) (listof (or/c material? image? p:pict?)))

      (define quest-f (list-ref (quests) (sub1 q-number)))

      (define q (handle-cards (quest-f)))
      
      (define ret ((policy q) course-or-num))

      (flatten ret)
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

(define (save-any thing name kind)
  (if (image? thing)
      (save-image thing (~a name ".png"))
      (save-pict  thing (~a name ".png") 'png))
  (void))





(define/contract (launcher-img thing)
  (-> launchable?
      p:pict?)

  (p:vc-append
   (p:scale (p:text "Use Scripts > launch") 2)
   (launcher-img-defined-launcher thing)))


(define (launcher-img-defined-launcher thing)
  (define module-name (~a (defined-launcher-package-name thing)))
  (define image-name  (~a (defined-launcher-id           thing)))
  
  (define i
    (p:scale
     (p:code (launch
              #,(p:colorize (p:text (string-replace module-name "ts-curric-" "")) "darkgreen")
              #,(p:colorize (p:text image-name) "darkgreen")))
     2))

  
  (p:frame
   (p:cc-superimpose
    (p:colorize
     (p:filled-rectangle (+ 10 (p:pict-width i))
                         (+ 10 (p:pict-height i))  )
     "white")                 i))  )


(define-syntax (define-launcher-function stx)
  (define d (syntax->datum stx))
  (define name (second d))
  (define f    (third d))

  (define package-name
    (findf
     (curryr string-prefix? "ts-curric-")
     (map ~a (explode-path (syntax-source stx)))))

  (define short-package-name
    (string->symbol
     (string-replace package-name "ts-curric-" "")))
  
  (datum->syntax stx
   `(begin

      (if (not (procedure? ,f))
          (error (~a "Must give a function to define-launcher-function"))
          (void))
    
      (provide ,name)
      (define ,name
        (defined-launcher-function
          ',short-package-name
          ',name
          ,f)))))


(define-syntax-rule (define-launcher-list name ls ...)
  (begin

    #;(if (not ((listof launchable?) (list ls ...)))
        (error (~a "All must be launchables in define-launcher-list"))
        (void))
    
    (provide name)
    (define name
      (defined-launcher-list
        (defined-launcher-package-name (first (list ls ...)))
        'name
        (list ls ...)))))



(define-syntax-rule (define-image-file name path image)
  (begin
    (define local-path  (build-path path (~a 'name)))
    (save-any image local-path 'png)

    (define backwards-path-elems (reverse (explode-path path)))
    (define package-name (second backwards-path-elems))
    (define dir-name     (first backwards-path-elems))

    (provide name)
    (define name (defined-image
                   (~a package-name)
                   (~a 'name)
                   image))))


(define-syntax-rule (define-racket-file name folder file-name)
  (begin
    (define path  (build-path folder file-name))

    (if (not (file-exists? path))
        (error (~a "File must exist in order to use define-racket-file: " file-name))
        (void))

    (define backwards-path-elems (reverse (explode-path path)))
    (define package-name (third backwards-path-elems))
    
    (provide name)
    (define name (defined-racket-file
                   package-name
                   'name
                   path))))



;The "path" param here is kind of silly.  I'd like to get rid of it.
;  Currently, it's used to track which module the webpage was defined in.
(define-syntax-rule (define-webpage name path url)
  (begin
    (define backwards-path-elems (reverse (explode-path path)))
    (define dir-name     (second backwards-path-elems))

    (provide name)
    (define name (defined-webpage
                   dir-name
                   (~a 'name)
                   url))))

;Defines how a defined-launcher will display to a student...
(define (student-display thing)
  (cond [(image? thing)                  thing]
        [(p:pict? thing)                 thing]
        [(procedure? thing)              (thing)]
        [(defined-image? thing)          (defined-image-image thing)]
        [(defined-webpage? thing)        (send-url (defined-webpage-url thing))] 
        [(defined-racket-file? thing)    (copy-paste-editor (defined-racket-file-path thing))]
        [(defined-launcher-list? thing)  (apply values (map student-display (defined-launcher-list-launchers thing) ))]
        [(defined-launcher-function? thing)  ((defined-launcher-function-f thing))]
        [(list? thing)                   (map student-display thing)]
        [else thing])  )


(define (copy-paste-editor path)
  (define my-racket:text%
    (class racket:text%
      (super-new)
    
      #;(define/augment (after-insert start end)
          (displayln "Hello"))))
  
  (define text-editor
    (new my-racket:text%))
  
  (send text-editor load-file
        path)
  
  (new editor-snip% 
       [editor text-editor]
       [min-width 200]	 
       [min-height 200]))


(define-syntax (launch stx)
  (define module (syntax->datum (second (syntax-e stx))))
  (define thing  (syntax->datum (third (syntax-e stx))))

  (define source (syntax-source stx))
  

  #;(define the-begin
    (if (not (eq? 'circuit-playground module)) ;Ugh, only works if the program has been run once...
        'begin ;Usually this
        'racket-begin ;Hack to make circuit-playground lang work -- because it annoyingly overrides 'begin'
        ))
  
  (if (or (equal? module '___)
          (equal? thing  '___))
      (datum->syntax stx '(displayln "Ummm. You were supposed to fill in the blanks: ____.  Try again!"))
      (datum->syntax stx
                     `(begin ;,the-begin
                        (displayln "One moment...")
                        (require ts-curric-common )
                        
                        (require ,(string->symbol
                                   (~a "ts-curric-" module)))
                        (student-display ,thing)
                        ))))


(define-syntax (require/provide-common stx)
  (datum->syntax stx
                 `(begin
                    (provide (all-from-out "common.rkt"))
                    (require "common.rkt"))))


(define-syntax (define-quests stx)
  (define qs (rest (map syntax->datum (syntax-e stx))))

  (datum->syntax stx
                 `(begin
                    (provide quests
                             (all-from-out ,@(map (λ(s) (~a s ".rkt")) qs)))

                    (require
                      ,@(map (λ(s) (~a s ".rkt")) qs))

                    (define (quests)
                      (list
                       ,@qs
                       )))))


(require racket/runtime-path)
(define-runtime-path images "images")

(define (scripts>launcher-img)
  (p:bitmap (build-path images "scripts-launch.png")))



(define-launcher-function hello-world  
  (lambda()
    (p:rotate
     (p:vl-append 10
                  (p:scale (p:text "Success!!!") 4)
                  (random-dude)
                  (p:scale (p:text "Usually there would be more here:") 2)
                  (p:scale (p:text "Sometimes code.  Sometimes a video.  Sometimes a mystery.") 2)
                  (p:scale (p:text "But for now, it's just a weird creature.") 2)
                  (p:scale (p:text "(HINT: For a surprise, try running the launch code again.)") 2))
     (/ (- (random) 0.5) 2)
     )))

(define (how-to-use-launcher-card)
  (activity-instructions "Launch Codes"
                         '()
                         (list                        
                          (instruction-basic "Open the launcher")
                          (instruction-basic "Fill in the blanks")
                          (instruction-basic (launcher-img hello-world))
                          (instruction-basic "Move your cursor to the end and press enter.")
                          (instruction-goal "the mystery success image"))
                         (scripts>launcher-img)))

(module+ test
  (how-to-use-launcher-card))





