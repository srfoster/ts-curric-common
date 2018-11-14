#lang racket

(provide transform-code
         extract-from-file
         read-lang-file
         define-syntax-class
         typeset-with-targets
         delete-this
         change-this
         note-this)

(require lang-file/read-lang-file)
(require syntax/parse)
(require pict)
(require pict/code)
(require (for-syntax racket))
(require (only-in ts-racket x-out))

(define-syntax (extract-from-file stx)
  (define file-name (second (syntax->datum stx)))
  (define thing (third (syntax->datum stx)))

  (datum->syntax stx
                 `(let ([ s (rest
                             (syntax-e
                              (fourth
                               (syntax-e
                                (read-lang-file ,file-name)))))])

                    
                    (syntax-parse s
                      [(any1 ... ,(string->symbol (~a "thing:" thing)) any2 ...)
                       #'thing]
                      [else #f]))))


(define (transform-code #:do (do identity)
                        #:if (rule (thunk* #f))
                        stx)
  (define parts (if (syntax? stx)
                    (syntax-e stx)
                    stx))

  (define recursive-call
    (λ(s)
      (transform-code #:do do #:if rule s)))

  (if (not (syntax? stx))
      stx
      (if (list? parts)
          (datum->syntax stx
                         (map recursive-call parts)
                         stx)
          (datum->syntax stx
                         (if (or (eq? rule parts)
                                 (and (procedure? rule)
                                      (rule parts)))
                             (do parts)
                             parts)
                         stx))))

;Rename this:
;  Takes a syntax tree and a list of (if,then pairs)
;  Returns a single main image and a list of sub images
;  Suitable for code+hints...
(define (typeset-with-targets stx . if-thens)

  (define hint-targets '())

  (define main-syntax
    (foldl (λ(next acc)
             (define f  (first next))
             (define do (second next))
             (transform-code acc
                             #:do (lambda(x)
                                       
                                    (define ret (do x))

                                    (set! hint-targets (cons ret hint-targets))
                                       
                                    ret)
                             #:if f))

           stx
           if-thens))
  
  
  (define main
    (typeset-code main-syntax))

  (values main
          hint-targets))


(require (prefix-in h: 2htdp/image))

(define (fix-datum maybe-i)
  (cond [(h:image? maybe-i) (bitmap maybe-i)]
        [(pict? maybe-i)    maybe-i]
        [else               (typeset-code (datum->syntax #f maybe-i))]))

(define (delete-this i)
  (code-align
   (x-out
    (frame #:color "red"
           (inset (fix-datum i) 10)))))

(define (change-this i)
  (code-align
   (frame #:color "red"
          (inset (fix-datum i) 10))))

(define (note-this i)
  (code-align
   (inset (fix-datum i) 10)))



(module+ test
  (define-syntax-class start-game-call
    (pattern ((~datum start-game) first:expr expr ... last:expr)))
  
  (define ret
    (extract-from-file (build-path "/Users/thoughtstem/Dev/Curriculum/ts-curric-game-engine/starter-files" "tsgd_runner_1.rkt")
                       start-game-call))

  (define new-ret
    (syntax-case ret (start-game)
      [(start-game first rest ... last)
       #`(start-game first
                     #,(datum->syntax #f 'SNIPE #'last)
                     rest
                     ...
                     last)]))


  (define-values (main hint-targets)
    (typeset-with-targets new-ret
                          (list 'SNIPE delete-this #;(replace-with #'YOU-BEEN-SNIPED!))))

  (typeset-code ret)
  (typeset-code new-ret)

  (require (only-in ts-racket code+hints hint))
  (code+hints main
              (list (first hint-targets) (hint "Replace this.  Customize the values...")))

  )
