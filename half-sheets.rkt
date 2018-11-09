#lang slideshow

(provide activity-move-resource)
(provide (rename-out [make-activity-instructions activity-instructions]))
(provide activity-edit-code)
(provide activity-repeat)
(provide with-award)
(provide choose)
(provide settings)
(provide pad)

(provide instruction-open)
(provide instruction-open-file)
(provide instruction-basic)
(provide instruction-goal)
(provide instruction-goal-side)
(provide instruction-folder)
(provide instruction-image)
(provide instruction-subtitle)
(provide instruction-qr)
(provide tagged-link)
(provide video-qr)
(provide code-qr)
(provide image-qr)
(provide search-qr)
(provide download-qr)

(provide ++)
(provide INSTRUCTOR_TERM
         FOLDER_ICON)

(provide super-flatten)
(provide is-reminder?)
(provide make-picts)
(provide generate-qrs)
(provide ts-magic-loader-img)

(provide codify)
(provide text-with-image)

(require simple-qr)
(require pict/shadow)
(require slideshow/text)
(require pict/color)
(require pict/code)
(require racket/runtime-path)

(current-font-size 40) 

(set-margin! 0)

(define-runtime-path images "images")
(define-runtime-path qrs "qrs")

(define (local-bitmap s)
  (bitmap (~a images "/" s)))

(define ts-magic-loader-img
  (local-bitmap "tsMagicLoader.png"))

(define save-my-work-image
  (local-bitmap "SAVE_MY_WORK.png"))

(define GALLIUM_ICON
 (scale (local-bitmap "galliumos.jpg") 0.2))

(define INSTRUCTOR_TERM "Game Master")

(define ++ string-append)

(define (pathify-url url)
  (++ (string-replace (string-replace url ":" "_" #:all? true) "/" "_" #:all? true)
      ".png"))

(define FOLDER_ICON
  (local-bitmap "folder-icon.png"))

(struct settings (bg avatar avatar-choice avatar-reminder))

(struct tagged-link (tag url) #:transparent)

(define (video-qr url)
  (tagged-link "video" url))

(define (code-qr url)
  (tagged-link "code" url))

(define (image-qr url)
  (tagged-link "image" url))

(define (search-qr url)
  (tagged-link "search" url))

(define (download-qr url)
  (tagged-link "download" url))




(struct instruction-basic (words) #:transparent)
(struct instruction-subtitle (words) #:transparent)
(struct instruction-image (path width height label) #:transparent)
(struct instruction-open (program) #:transparent)
(struct instruction-open-file (file program) #:transparent)
(struct instruction-goal (words) #:transparent)
(struct instruction-goal-side (words) #:transparent)

(struct instruction-qr (url) #:transparent)

(struct instruction-folder (path) #:transparent)


(struct activity-instructions (title refs steps video-url) #:transparent)

(define (make-activity-instructions title refs steps video-url)
  (activity-instructions (string-titlecase title) refs steps video-url))


(struct activity-edit-code (file-path) )
(struct activity-move-resource (src dest) #:super struct:activity-instructions)
(struct activity-repeat (times) #:super struct:activity-instructions)

(struct with-award (award activity))
(struct choose (number activities))

(define (codify s)
  (pad
   (colorize
    (with-size 35
      (with-font "Courier" (t s)))
    "blue")
   20
   cb-superimpose))

(define (text-with-image . things)
  (define (maybe-convert-string thing)
    (if (string? thing)
        (t thing)
        thing))
  
  (apply hb-append (map maybe-convert-string things)))

(define/contract (logo-for s)
  (-> string? pict?)
  (scale-to-fit
   (match (string-downcase s)
     ["piskel"   (bitmap (build-path images "piskel.png"))]
     ["drracket" (bitmap (build-path images "drracket.png"))]
     ["chrome"   (bitmap (build-path images "chrome.png"))]
     [else (ghost (rectangle 0 0))])
   40 40 #:mode 'preserve))

(define (string->open-pict s)
  (hc-append 5
   (t "Open ")
   (t s)
   (logo-for s)))

(define (instruction-open->pict i)
  (string->open-pict (instruction-open-program i)))


(define (bgify i c)
  (cc-superimpose
   (filled-rectangle (pict-width i) (pict-height i) #:color c #:draw-border? #f)
   i))


(define (instruction-goal->pict i)
  (if (not i) (blank 0)
      (let ([title (bt (++ "Show " INSTRUCTOR_TERM "..."))])
        (shadow-frame
         (pad 
          (colorize
           (vc-append 0
                      title
                      (para #:width (+ (pict-width title) 150)
                            #:align 'center
                            (++ "..." (add-period-if-necessary (lowercase-first-letter (fix-quotes (instruction-goal-words i)))))
                            ))
           "black")
          0
          cc-superimpose)))))

(define (instruction-goal-side->pict i)
  (if (not i) (blank 0)
      (let ([title (bt (++ "Show " INSTRUCTOR_TERM "..."))])
        (shadow-frame
         (pad 
          (colorize
           (vc-append 0
                      title
                      (para #:width (pict-width title)
                            (++ "..." (add-period-if-necessary (lowercase-first-letter (fix-quotes (instruction-goal-side-words i)))))
                            ))
           "black")
          0
          cc-superimpose)))))

(define (is-eos-punctuation? s)
  (or
   (string=? s ".")
   (string=? s "?")
   (string=? s "!")))

(define (add-period-if-necessary s)
  (let* ([last-letter (substring s (- (string-length s) 1))])
    (if (is-eos-punctuation? last-letter)
        s
        (++ s "."))))

(define (lowercase-first-letter s)
  (let* ([lowercase-first-letter (string-downcase (substring s 0 1))]
         [rest-of-string (substring s 1)])
    (++ lowercase-first-letter rest-of-string)))

(define (instruction-subtitle->pict i)
  (if (not (instruction-subtitle? i))
      (blank 0)
      (colorize (t (instruction-subtitle-words i)) (dark "gray"))))

(define (instruction-folder->pict i)
  (path->pict (instruction-folder-path i)))

(define (instruction-image->pict i)
  (hc-append 10
             (scale-to-fit (bitmap (instruction-image-path i))
                           (instruction-image-width i)
                           (instruction-image-height i))
             (t (instruction-image-label i))))


(define (instruction-qr->pict i)
  (vl-append
          (scale (display-video-qr (instruction-qr-url i)) 0.75)))


(define (instruction-open-file->pict i)
  (arrow-between 100 "Open with"
                  (path->pict (instruction-open-file-file i))
                  (t (instruction-open-file-program i))))


(define (fix-quotes s)
  (string-replace
   (string-replace s " '" " ‘")
   "' "
   "’ "))

(define (format-basic-instruction i)
  (let* ([words (instruction-basic-words i)])
    (cond [(string? words) (fix-quotes words)]
          [else words])))

(define (instruction->pict i)
  (item #:width 600
        #:bullet (if (or
                      (instruction-basic? i)
                      (instruction-open? i)) bullet (blank 0))
        (cond [(instruction-subtitle? i)    (blank 0)] ;These don't render in the normal place
              [(instruction-goal? i)        (blank 0)] ;Nor do these
              [(instruction-goal-side? i)   (instruction-goal-side->pict i)]
              [(instruction-basic? i)       (format-basic-instruction i)]
              [(instruction-open? i)        (instruction-open->pict i)]
              [(instruction-open-file? i)   (instruction-open-file->pict i)]  
              [(instruction-folder? i)      (instruction-folder->pict i)]
              [(instruction-image? i)       (instruction-image->pict i)]
              [(instruction-qr? i)          (instruction-qr->pict i)]
              [else (t "ERROR: Unknown instruction")])))

(define (activity->pict a)
  (cond [(string? a) (para a)]
        [(activity-move-resource? a)  (activity-move-resource->pict a)]
        [(activity-repeat? a)         (activity-repeat->pict a)]
        [(activity-instructions? a)   (activity-instructions->pict a)]
        [else (t "ERROR: Unknown activity")]))

(define (url-without-dashes? s)
  (displayln s)
  (and (string? s)
       (or (= 0 (string-length s))
           (and 
            (is-url? s)
            (not (string-contains? s "-"))))))

(define #;/contract (write-out-qr s)
  #;(-> url-without-dashes? void)

  (displayln s) 
  (displayln (pathify-url s))
  (qr-write s
                  (~a (path->string qrs) "/" (pathify-url s))
                  ))

(define (_generate-qr a f)
  
  #;(displayln "We generatin a qr..")
  (let* ([maybe-tagged-url (f a)]
         [url (cond [(string? maybe-tagged-url) maybe-tagged-url]
                    [(tagged-link? maybe-tagged-url) (tagged-link-url maybe-tagged-url)]
                    [else maybe-tagged-url])])
    (if (string? url)
        (write-out-qr url)
        (displayln "Tried to _generate-qr for a non string.  Mistake?")
   
        )))




(define (generate-qr a)
  (cond [(activity-instructions? a) (_generate-qr a activity-instructions-video-url)]
        [(instruction-qr? a) (_generate-qr a instruction-qr-url)]
        ))


(define (path-item a b)
  (let* ([is-file (string-contains? a ".")]
         [folder-bullet (scale-to-fit (local-bitmap "folder-icon.png") 20 20)]
         [file-bullet (scale-to-fit (local-bitmap "file-icon.png") 20 20)]
         [bullet (if is-file file-bullet folder-bullet)])
    (item  #:bullet bullet
           #:width 50
           (if is-file
               a
               (++ a "/"))
           b)))

(define (path->pict p)
  (frame
   (pad
    (foldl path-item (blank 0) (reverse (string-split p "/")))
    10
    cc-superimpose)))




(define (display-video-qr a)
  #;(displayln "display-video-qr")
  (let* ([maybe-tag a]
         [icon (if (tagged-link? maybe-tag)
                   (scale-to-fit (local-bitmap (++ (tagged-link-tag maybe-tag) "-icon.png")) 20 20)
                   (blank 0))]
         [url (cond [(tagged-link? maybe-tag) (tagged-link-url maybe-tag)]
                    [else maybe-tag])])
    (if (eq? url "")
        (blank 0)
        (ct-superimpose
         (qr-or-image url)
         icon))))

(define (find-subtitle-instruction a)
  (findf instruction-subtitle? (activity-instructions-steps a)))

(define (find-goal-instruction a)
  (findf instruction-goal? (activity-instructions-steps a)))

(define (activity-header a)
  (vc-append 15
    (vc-append 
               (scale (bt (activity-instructions-title a)) 1.5)
               (instruction-subtitle->pict (find-subtitle-instruction a)))
    (activity->reminders a)))

(define (activity-move-resource->pict a)
  (vc-append 20
   (activity-header a)
   (ht-append
    (display-video-qr (activity-instructions-video-url a))
    (vc-append
     (arrow-between 100 "Move"
                    (display-video-qr (activity-move-resource-src a))
                    (path->pict (activity-move-resource-dest a)))
     (apply (curry vl-append 10)
            (map instruction->pict
                 (activity-instructions-steps a)))))
   (instruction-goal->pict (find-goal-instruction a))))


(define (is-url? s)
  (or
   (string-contains? s "http")
   (string-contains? s "file")))

(define (qr-or-image p)
  #;(displayln p) 
  #;(displayln "qr-or-image")
  
  (cond [(pict? p) (pad p 20 cc-superimpose)]
        [(= 0 (string-length p)) (blank 0)]
        [(not (is-url? p)) (pad (bitmap p) 20 cc-superimpose)]
        [else (qr-or-path p)]))

(define (qr-or-path a)
  #;(displayln "qr-or-path")
  
  (let ([url (if (tagged-link? a) (tagged-link-url a) a)])
    (if (eq? url "")
        (blank 0)
        (if (is-url? url)
            (url->qr url)
            (path->pict url)))))

(define (url->qr url)
  #;(displayln "url->qr")

  (define img-path
    (~a (path->string qrs) "/" (pathify-url url)))

  (and (not (file-exists? img-path))
       (write-out-qr url)
       )

  (define qr-img
    (bitmap img-path))
  
  (cb-superimpose
       (scale-to-fit qr-img 250 250)
       (colorize (scale (hc-append (t "Scan with ") (string->open-pict "Learn")) 0.5) "gray")))


(define (activity->ref-pic a)
  (bgify (pad (t (activity-instructions-title a)) 10 cc-superimpose)
         "gray"))

(define (activity->reminders a)
  (let ([refs (activity-instructions-refs a)])
    (if (empty? refs)
        (blank 0)
        (colorize (scale (hc-append 10 (t "Reminders:") (apply (curry hc-append 10) (map activity->ref-pic refs))) 0.5 )
                  "black"))))

(define (activity-instructions->pict a)
    
  (vc-append 5
             (activity-header a)
             (hc-append
              (display-video-qr (activity-instructions-video-url a))
              (apply ((curry vl-append) 10)
                     (map instruction->pict
                          (activity-instructions-steps a))))
             (instruction-goal->pict (find-goal-instruction a))))

(define (arrow-between dist label pict-a pict-b)
  (let ([label (scale-to-fit (t label) (/ dist 2) (/ dist 2))])
    (pin-arrow-line 30 (hc-append dist pict-a pict-b)
                    pict-a rc-find
                    pict-b lc-find
                    #:label label
                    #:x-adjust-label (+ (pict-width label) (- (/ (pict-width label) 2)  dist))
                    #:solid? #t
                    #:line-width 3)))



(define (arch-over-arrow dist pict-a pict-b)
  (pin-arrow-line 30 (hc-append dist pict-a pict-b)
                   pict-a ct-find
                   pict-b ct-find
                   #:start-angle (/ pi 2)
                   #:end-angle   (/ pi -2)
                   #:solid? #t
                   #:line-width 3))

(define (arch-under-arrow dist pict-a pict-b)
  (pin-arrow-line 30 (hc-append dist pict-a pict-b)
                   pict-b cb-find
                   pict-a cb-find
                   #:start-angle (/ pi -2)
                   #:end-angle   (/ pi 2)
                   #:solid? #t
                   #:line-width 3))


(define (open-arrow dist pict-a pict-b)
  (arrow-between dist "Open" pict-a pict-b))

(define (activity-repeat->pict a)
  (let* [(main
         (apply ((curry vl-append) 10)
                         (map instruction->pict
                              (activity-instructions-steps a))))
         (times (number->string (activity-repeat-times a)))
        (label
         (para #:width 200 (++ "You can earn $s for this " times "x")))
        (label-with-arrows (hc-append (cc-superimpose
                                       (scale-to-fit (local-bitmap "repeat-icon.png") 100 100)
                                       (t times))
                                      label))]
    (vc-append 20
               (activity-header a)
               (ht-append (vc-append label-with-arrows (display-video-qr (activity-instructions-video-url a))) main)
               (instruction-goal->pict (find-goal-instruction a)))))

(define (pad i w f)
  (f i (blank (+ (pict-width i) w)
              (+ (pict-height i) w))))


(define (make-bg sequence settings wrapper)
  (let ([avatar (if (is-reminder? sequence (with-award-activity wrapper))
                    (settings-avatar-reminder settings)
                    (settings-avatar settings))])
    (lb-superimpose
     (settings-bg settings)
     (pad avatar 20 rt-superimpose))))


;;Creates a bunch of bitmap files
(define (generate-qrs seq)
  (map generate-qr (map with-award-activity (flatten seq))))

(define (repeat f i times)
  (foldl (lambda (_ n) (f i n)) (blank 0) (range times)  ))

(define (award-amount->pict amount)
  (let ([icon (scale (local-bitmap "coin.png") 0.5)])
   (vc-append (colorize
               (t (string-append (if (> 0 amount) "-" "") "$" (number->string (abs amount))))
               (cond [(= 0 amount ) "gray"]
                     [(> 0 amount ) "red"]
                     [else "black"]))
              (repeat hc-append icon (abs amount)))))

(define (make-award-overlay amount bg)
  (rb-superimpose
   (blank (pict-width bg) (pict-height bg))
   (pad (award-amount->pict amount) 100 lt-superimpose)))


(define (same-title a1 a2)
  (eq? (activity-instructions-title a1)
       (activity-instructions-title a2)))

;Something is a reminder if there is some activity
;that references.
(define (is-reminder? sequence activity)
  (not
   (empty?
    (filter (lambda (s)
              (< 0 (length (filter (curry same-title activity) (activity-instructions-refs (with-award-activity s))))))
            (super-flatten sequence)))))


(define (wrapper->pict sequence settings wrapper)

  (let ([middle-ground (activity->pict (with-award-activity wrapper))]
        [foreground (make-award-overlay (with-award-award wrapper) (settings-bg settings))]
        [background (make-bg sequence settings wrapper)])
     (cc-superimpose
      background
      middle-ground
      foreground)))

;Swaps the normal and the choice avatar
(define (flip-avatars s)
  (settings (settings-bg s)
            (settings-avatar-choice s)  ;Swap this
            (settings-avatar s)         ;and this
            (settings-avatar-reminder s)))

(define (choice->pict sequence settings n wrappers)
  (let ([new-settings (flip-avatars settings)])
    (map (curry wrapper-or-choice->pict sequence new-settings) wrappers)))

(define (wrapper-or-choice->pict sequence settings wrapper)
  (match wrapper
    [(with-award n activity) (wrapper->pict sequence settings (with-award n activity))]
    [(choose n wrappers)     (choice->pict sequence settings n wrappers)]
    [x                       (error "Something wasn't either a with-award or a choice")]))

(define (super-flatten seq)
  (flatten
   (map (lambda (s)
         (match s
             [(choose n wrappers) (super-flatten wrappers)]
             [x x]))
       (flatten seq))))

(define (add-page-number pict n quest color)
  (cc-superimpose
    pict
    (rb-superimpose
     (ghost pict)
     (cbl-superimpose 
      (filled-rectangle 140 65 #:color "White" #:border-color "Black" #:border-width 3)
      (colorize
       (pad (with-size 50 (t (++ quest (number->string n)))) 25 cc-superimpose)
       color)))))

(define (make-picts color quest seq settings)
  
  
  (let* ([picts (flatten
                 (map (curry wrapper-or-choice->pict seq settings)
                      (flatten seq)))])
       (map add-page-number
            picts
            (range 1 (+ 1(length picts)))
            (make-list (length picts) quest)
            (make-list (length picts) color)
            )))

