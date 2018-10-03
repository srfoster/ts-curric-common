#lang slideshow

(provide generic-rewards)
(require pict/color)
(require pict/shadow)

(require simple-qr)
(require "half-sheets.rkt")

(define (circlify color i)
  (cc-superimpose
   (inset (shadow (disk (+ 10 (pict-width i)) #:color color #:border-width 5) 10) 10)
   i))

(define (augment-img level img)
  (let* ([color (cond [(not level) "red"]     ;False is considered "infinite"
                      [(= 1 level) "cyan"]
                      [(> 3 level) "orange"]
                      [(> 5 level) "yellow"]
                      [(> 10 level) "red"]
                      [else "white"])])
  (circlify color (pad img 10 cc-superimpose))))

(define (priviledge title description img uses cost #:extra [extra-instructions '()])
  (let* ([usage-phrase (if (not uses)
                           "as many times as you want"
                           (++ (number->string uses) " time(s)"))])
    (with-award cost (activity-instructions (string-titlecase title)
                                            '()
                                            (append
                                             extra-instructions
                                             (list
                                              (instruction-basic (++ "You may use this power " usage-phrase))
                                              (instruction-subtitle description)))
                                            (augment-img uses (scale-to-fit (bitmap img) 75 75))))))

(define (priviledge-sequence title description img [costs '(-1 -3 -5)] [uses '(1 3 #f)] #:extra [extra-instructions '()])
  (map (curry priviledge title description img #:extra extra-instructions) uses costs))

(define (game-chooser game1 game2)
  (priviledge-sequence (++ "Choose: " game1 " vs " game2 "!")
                       (++ "During game time today, you may choose the class game.")
                       "king-crown-icon.png"
                       '(-3 -5 -7)
                       '(1  2  3)))

(define (game-chooser-sequence games)
  (map (curry apply game-chooser) (combinations games 2)))
 
(define generic-rewards
  (list
   (priviledge-sequence "Assistant Game Master"
                        "You may check another student's work on a task you've done already."
                        #:extra (list (instruction-basic "You may give them the appropriate number of dollars if they have done the task correctly.")
                                      (instruction-basic "You may earn $1 each time you do this."))
                        "king-crown-icon.png"
                        '(-3 -5 -10)
                        '(1  2  #f))
   (game-chooser-sequence '("Wah" "Ninja" "My Neighbor" "Human Knot"))
   (priviledge-sequence "Hall Pass"
                        "You can go to the bathroom at any time."
                        "king-crown-icon.png")
   (priviledge-sequence "Run the Market"
                        "You may run the market during market time."
                        "king-crown-icon.png"
                        '(-3 -5 -7)
                        '(1  2  3))
   (priviledge-sequence "Helpful Bonus"
                        "If you help someone on a task, you also earn dollars."
                        "king-crown-icon.png")
   (priviledge-sequence "Line Leader"
                        "You get to lead (or co-lead) the line."
                         #:extra (list (instruction-basic "If you and another player use this power at the same time, you must both lead the line."))
                        "king-crown-icon.png")
   (priviledge-sequence "Be the Banker"
                        "You may exchange another student's $1s for $5s or $10s."
                        #:extra (list (instruction-basic "You earn $1 every time you do this.")
                                      (instruction-basic "If you are caught stealing, you will lose all your money and anything you have previously earned."))
                        "king-crown-icon.png")
   (priviledge-sequence "Passer Outer"
                        "You may help the teacher pass out or pick up equipment."
                        "king-crown-icon.png")
   (priviledge-sequence "Double Your Earnings"
                        "On a task of your choice, you may earn double dollars"
                        "king-crown-icon.png"
                        '(-5 -10 -20))))




;(map slide (make-picts rewards BG-BLUE YODA-AVATAR))