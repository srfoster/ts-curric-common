#lang racket

(require quickscript/script)

;; See the manual in the Scripts>Manage Scripts>Help menu for more information.

(define-script launch
  #:label "launch"
  #:output-to clipboard
  (λ(selection #:definitions defs #:frame fr #:interactions ints #:file f)
    (send fr ensure-rep-shown #t)

    (define def-text  (send defs get-text))
    (define int-text  (send ints get-text))

    (define require-code '(require ts-curric-common))

    (define guess (cond
                    [(string-contains? def-text "game-engine")        'game-engine-rpg]
                    [(string-contains? def-text "circuit-playground") 'circuit-playground]
                    [else '___]))
    
    (define launch-code (~a "(launch"
                            "\n    "
                            guess
                            "\n    "
                            "___)"))

    (define to-inject
      (~a require-code "\n  " launch-code))

    (send ints insert-port
          (open-input-string to-inject))  )  )
