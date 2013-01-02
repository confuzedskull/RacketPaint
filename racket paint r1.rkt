#lang scheme
(require picturing-programs)

(define draw-space (rectangle 500 500 "outline" "black"))
(define white-background (rectangle 500 500 "outline" "white"))
(define black-background (rectangle 500 500 "solid" "black")) 

;GRID
(define cell (rectangle 10 10 "outline" "black"))

(define (row width image)
  (if (= width 1)
      image
      (beside image (row (- width 1) image))))

(define (column height image)
  (if (= height 1)
      image
      (above image (column (- height 1) image))))

(define (grid width height image) 
  (cond [(or (= width 0)(= width 1)) (column height image)]
        [(or (= height 0)(= height 1)) (row width image)]
        [else (beside (column height image) (grid (- width 1) height image))]))

;PALETTE
(define palette-1 (frame (rectangle 50 50 "solid" "red")))
(define palette-2 (frame (rectangle 50 50 "solid" "orange")))
(define palette-3 (frame (rectangle 50 50 "solid" "yellow")))
(define palette-4 (frame (rectangle 50 50 "solid" "green")))
(define palette-5 (frame (rectangle 50 50 "solid" "blue")))
(define palette-6 (frame (rectangle 50 50 "solid" "purple")))
(define palette-7 (frame (rectangle 50 50 "solid" "brown")))
(define palette-8 (frame (rectangle 50 50 "solid" "black")))
(define palette-9 (frame (rectangle 50 50 "solid" "white")))
(define palette-10 (circle 25 "outline" "black"))

(define color-palette (beside (beside (beside (beside (beside palette-1 palette-2) 
                        (beside palette-3 palette-4)) 
                (beside palette-5 palette-6)) 
        (beside palette-7 palette-8))
  (beside palette-9 palette-10)))


(define scene-1 (above color-palette draw-space))



(define grid-scene (above color-palette (grid 50 50 cell)))

;HELP SCREEN
(define help_text (above (text "Racket Paint by James Nakano" 20 "black")
                           (above (text "Controls:" 15 "black")                     
(above/align "left" (text "up/down arrow keys - change opacity." 10 "black")
(above/align "left" (text "left/right arrow keys - change brush diameter" 10 "black")
(above/align "left" (text "(g) - make grid" 10 "black")
(above/align "left" (text "(c) - clear screen" 10 "black")
(above/align "left" (text "(u) - undo" 10 "black")
(above/align "left" (text "(h) - open help screen" 10 "black")
(above/align "left" (text "(s) - save drawing to racket drawing.png" 10 "black")
(above/align "left" (text "(+) - zoom in" 10 "black")
(above/align "left" (text "(-) - zoom out" 10 "black")
             (text "press u to undo this screen" 10 "blue")))))))))))))

(define help_screen (place-image help_text 250 250 white-background))
(define help-scene (above color-palette help_screen))
;Structure definitions
(define scene_1 (list help_screen draw-space 0))
(define-struct settings (image help grid))
;(define-struct dot (diameter opacity color x y scene settings))
(define-struct dot (diameter opacity color x y scene))

(define current (make-dot 10 150 "black" 475 25 scene_1))
(define (colored-dot diameter opacity color) (circle diameter opacity color))
(define (colored-square diameter opacity color) (rectangle diameter diameter opacity color))
(define (change-color current new-color) (make-dot (dot-diameter current)
                                                   (dot-opacity current)
                                                   new-color
                                                   (dot-x current)
                                                   (dot-y current)                                                  
                                                   (dot-scene current)))
(define (change-posn current x y) (make-dot (dot-diameter current)
                                            (dot-opacity current)
                                            (dot-color current) 
                                            x y (dot-scene current)))


(define (render-scene current new-scene) (make-dot (dot-diameter current)
                                                   (dot-opacity current)
                                                   (dot-color current)
                                                    (dot-x current) (dot-y current)
                                                    new-scene))
;DRAW HANDLE                                                      
(define (draw-on-scene current)
  (place-image (colored-dot (dot-diameter current)
                            (dot-opacity current)
                            (dot-color current)) (dot-x current) (dot-y current) (first (dot-scene current))))
;(place-image (colored-square (dot-diameter current)
 ;                           (dot-opacity current)
  ;                          (dot-color current)) (dot-x current) (dot-y current) (first (dot-scene current))))

(define (draw_on_scene current)
 (overlay/align "left" "top"
                (place-image (colored-dot (dot-diameter current)
                            (dot-opacity current)
                            (dot-color current)) (dot-x current) (dot-y current) color-palette)
                
                ;(place-image (colored-square (dot-diameter current)
                 ;           (dot-opacity current)
                  ;          (dot-color current)) (dot-x current) (dot-y current) color-palette)
  (draw-on-scene current)))

;MOUSE CONTROL
(define (mouse-handle current x y mouse-event)
  (cond [(string=? mouse-event "button-up") 
         (cond [(<= y 50)
             (change-color current (get-pixel-color x y color-palette))] 
        [else (change-posn current 475 25)])]
        [(and (string=? mouse-event "button-down") (> y 50))
         (render-scene current (cons (draw-on-scene (change-posn current x y)) (dot-scene current)))]        
        [(string=? mouse-event "drag")
        (render-scene current (cons (draw-on-scene (change-posn current x y)) (dot-scene current))) ]
        [else current]))

;KEYBOARD CONTROL
(define (increase-diameter current)(make-dot (cond [(< (dot-diameter current) 25)
                                                    (+ (dot-diameter current) 1)]
                                                   [else (+ (dot-diameter current) 0)])  (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current)))

(define (decrease-diameter current) (make-dot (cond [(> (dot-diameter current) 1)
                                                     (- (dot-diameter current) 1)]
                                                    [else (- (dot-diameter current) 0)]) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current)))

(define (increase-opacity current) (make-dot (dot-diameter current) (cond [(< (dot-opacity current) 255)
                                                                           (+ (dot-opacity current) 1)]
                                                                          [else (+ (dot-opacity current) 0)]) (dot-color current) (dot-x current) (dot-y current) (dot-scene current)))

(define (decrease-opacity current) (make-dot (dot-diameter current) (cond [(> (dot-opacity current) 1)
                                                                           (- (dot-opacity current) 1)]
                                                                          [else (- (dot-opacity current) 0)]) (dot-color current) (dot-x current) (dot-y current) (dot-scene current)))

(define (key-handle current key)
 (cond [(string=? key "left") (decrease-diameter current)]
        [(string=? key "right") (increase-diameter current)]
        [(string=? key "up") (increase-opacity current)]
        [(string=? key "down") (decrease-opacity current)]
        [(and (string=? key "u") (image? (first (rest (dot-scene current))))) (render-scene current (rest (dot-scene current)))]
        [(string=? key "h") (render-scene current (cons help-scene (dot-scene current)))]
        [(string=? key "g") (render-scene current (cons grid-scene (dot-scene current)))]
        [(string=? key "c") (render-scene current (cons scene-1 (dot-scene current)))]
        [(string=? key "s") (save-image (first (dot-scene current)) "racket drawing.png")]
        [(string=? key "add") (render-scene current (cons (scale 1.25 (first (dot-scene current))) (dot-scene current)))]
        [(string=? key "subtract") (render-scene current (cons (scale 0.75 (first (dot-scene current))) (dot-scene current)))]
        [else current]))

;PROGRAM FUNCTION
(define draw (big-bang current                              
            (on-draw draw_on_scene)
            (on-key key-handle)
            (on-mouse mouse-handle)))

