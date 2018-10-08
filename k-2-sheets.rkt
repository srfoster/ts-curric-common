#lang rosette

(provide (rename-out [make-panel panel])
         layout-panels
         render-panels
         titled-image-panel
         solid
         titled-image
         auto-layout )

(require 2htdp/image)
(require lang/posn)
(require ts-racket)

(module+ test
  ;Step 1: define your panels, with their min/max constraints...
  (define panels
    (list
     (make-panel (titled-image "First" (square 50 'solid 'red))     500 500 500 500)
     (make-panel (solid "darkred")); 250 500 250 500)
     (make-panel (solid "orange"));  750 250 750 250)
     (make-panel (solid "darkorange"))
     (make-panel (solid "yellow"))  
     (make-panel (solid "green"))
     (titled-image-panel "Last"
                         (star 100 'solid 'darkgreen))
     ;   (make-panel "blue")
     ;   (make-panel "darkblue")
     ))

  ;Step 2: Ask the solver to find a layout for you.
  (define answer (layout-panels panels))


  ;Step 3: Render the panels...
  (render-panels answer))







(define cell-size 50)
(define title-font-size 18)
(define padding 20)
(define page-width (* 20 cell-size))
(define PAGE-HEIGHT (* 25 cell-size))


(struct panel
  (x y w h min-width min-height max-width max-height bg)
  #:transparent)

(define (auto-layout #:page-height (page-height PAGE-HEIGHT) . ps)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (displayln exn)
                     (error "Couldn't fit all that on one page!"))])
      (render-panels #:page-height page-height
       (layout-panels #:page-height page-height ps))))

(define (titled-image-panel #:fill (color 'white)
                            title image
                            (max-w page-width)
                            (max-h PAGE-HEIGHT))

  (define title-preview (text title title-font-size 'black))
  (define title-width (image-width title-preview))
  (define title-height (+ padding (image-height title-preview)))

  (define min-w (max (image-width image) (+ padding title-width)))
  (define min-h (+ (image-height image) title-height))
  
  (make-panel (titled-image #:fill color
                            title image)
              (min min-w max-w)
              (min min-h max-h)
              (max max-w min-w)
              (max max-h min-h)))

(define (solid c)
  (thunk* (square 50 'solid c)))

(define (titled-image #:fill (color 'transparent) title img)
  (λ(p)
    (define title-img (text title title-font-size 'black))

    (define title-height (+ padding
                            (image-height title-img)))
    
    (above
     (overlay
      title-img
      (overlay
       (rectangle (panel-w p) title-height 'outline 'black)
       (rectangle (panel-w p) title-height 'solid 'lightgray)))
     (overlay
      img
      (rectangle (panel-w p)
                 (- (panel-h p) title-height)
                 'outline 'black)
      (rectangle (panel-w p)
                 (- (panel-h p) title-height)
                 'solid color)))))

(define (make-panel bg
                    (min-width  cell-size)
                    (min-height cell-size)
                    (max-width  page-width)
                    (max-height PAGE-HEIGHT))
  (define-symbolic* x y w h integer?)
  
  (panel x y w h min-width min-height max-width max-height bg))


(define (render-panels #:page-height (page-height PAGE-HEIGHT) panels)
  (define bg (rectangle (+ 2 page-width)
                        (+ 2 page-height)
                        'outline
                        'darkgray))

  (define (->panel l)
    (define main-bg
      (rectangle (panel-w l)
                 (panel-h l)
                 'outline
                 'black))

    (define content
      ((panel-bg l) l))
    
    (overlay
     content
     main-bg))

  (define (->pos l)
    (make-posn (panel-x l)
               (panel-y l)))

  (place-images/align
   (map ->panel panels)
   (map ->pos   panels)
   "left" "top" 
   bg))


(define (layout-panels #:page-height (page-height PAGE-HEIGHT) panels)
  (current-bitwidth #f)


  ;No panel can be larger than the page...
  (map (λ(p)
         (assert (<= (panel-w p) page-width))
         (assert (<= (panel-h p) page-height)))
       panels)


  ;No panel can be smaller than the min panel size...
  (map (λ(p)
         (assert (>= (panel-w p) cell-size))
         (assert (>= (panel-h p) cell-size))
       
         (assert (>= (panel-w p) (panel-min-width p)))
         (assert (>= (panel-h p) (panel-min-height p)))

         (assert (<= (panel-w p) (panel-max-width p)))
         (assert (<= (panel-h p) (panel-max-height p))))
       panels)


  ;All panels are positioned on a grid layout
  (map (λ(p)
         (assert (= (remainder (panel-x p) cell-size) 0))
         (assert (= (remainder (panel-y p) cell-size) 0)))
       panels)

  ;No panels can go off the page
  (map (λ(p)
         (assert (<= (+ (panel-x p)
                        (panel-w p))
                     page-width))
       
         (assert (<= (+ (panel-y p)
                        (panel-h p))
                     page-height)))
       panels)

  ;Panels must start on the page
  (map (λ(p)
         (assert (>= (panel-x p) 0))
         (assert (>= (panel-y p) 0)))
       panels)


  ;Panels may not overlap

  (define panel-pairs
    (combinations panels 2))

  (map (λ(ps)
         (define p1-x (panel-x (first ps)))
         (define p1-y (panel-y (first ps)))
         (define p1-w (panel-w (first ps)))
         (define p1-h (panel-h (first ps)))       

         (define p2-x (panel-x (second ps)))
         (define p2-y (panel-y (second ps)))
         (define p2-w (panel-w (second ps)))
         (define p2-h (panel-h (second ps)))
       
         (define min-x (min p1-x p2-x))
         (define max-x (max (+ p1-x p1-w)
                            (+ p2-x p2-w)))

         (define x-overlap
           (< (- max-x min-x)
              (+ p1-w p2-w)))

         (define min-y (min p1-y p2-y))
         (define max-y (max (+ p1-y p1-h)
                            (+ p2-y p2-h)))
         (define y-overlap
           (< (- max-y min-y)
              (+ p1-h p2-h)))

         (assert  (=> x-overlap
                      (! y-overlap)))
         (assert  (=> y-overlap
                      (! x-overlap))))
       panel-pairs)

  ;Panels should be in order

  (define (rotate l)
    (append (rest l)
            (list (first l))))


  (define (comes-after-previous p)
    (define index (index-of panels p))

    (define previous (take panels index))

    (define p1-x (panel-x p))
    (define p1-y (panel-y p))
    (define p1-w (panel-w p))
    (define p1-h (panel-h p))

    (define (far-y p)
      (+ (panel-y p)
         (panel-h p)))

    (define (far-x p)
      (+ (panel-x p)
         (panel-w p)))
  

    (define max-far-y
      (apply max (map far-y previous)))

    (define max-far-x
      (apply max (map far-x previous)))

    #;(assert #t)
    (assert (or (= p1-x 0)
                (= p1-x (far-x (last previous)))))
  
    (assert (or (= p1-y (panel-y (last previous)))
                (= p1-y max-far-y)))
    )

  (assert (= 0 (panel-x (first panels))))
  (assert (= 0 (panel-y (first panels))))

  (map comes-after-previous (rest panels))

  #;(define (area p)
      (* (panel-w p)
         (panel-h p)))

  #;(define areas (map area panels))

  ;Maximizing by area seems to be broken.
  ;But maximizing all widths and heights seems to work...
  (define sol
    (optimize #:maximize (append (map panel-w panels)
                                 (map panel-h panels))
              #:guarantee (assert #t)))

  (define answer
    (evaluate
     panels
     sol))

  answer)

