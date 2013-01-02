#lang scheme
(require picturing-programs)

(define draw-space (rectangle 500 500 0 "white"))
(define white-background (rectangle 500 500 "solid" "white"))
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

(define (posn-color x y) (cond [(and (<= x 50 ) (<= y 50)) "red"]
                             [(and (<= x 100) (<= y 50)) "orange"]
                             [(and (<= x 150) (<= y 50)) "yellow"]
                              [(and (<= x 200) (<= y 50)) "green"]
                              [(and (<= x 250) (<= y 50)) "blue"]
                              [(and (<= x 300) (<= y 50)) "purple"]
                              [(and (<= x 350) (<= y 50)) "brown"]
                              [(and (<= x 400) (<= y 50)) "black"]
                              [(and (<= x 450) (<= y 50)) "white"]
                              [else (get-pixel-color x y scene-1)]))

(define grid-scene (above color-palette (grid 50 50 cell)))

;HELP SCREEN
(define help_text (above (text "Racket Paint by James Nakano" 20 "black")
                           (above (text "Controls:" 15 "black")                     
(above/align "left" (text "up/down arrow keys - change opacity." 10 "black")
(above/align "left" (text "left/right arrow keys - change brush diameter" 10 "black")
(above/align "left" (text "(g) - make grid" 10 "black")
(above/align "left" (text "(c) - clear screen" 10 "black")
(above/align "left" (text "(h) - open help screen" 10 "black")
(above/align "left" (text "(s) - save drawing to racket drawing.png" 10 "black")
(above/align "left" (text "(+) - zoom in" 10 "black")
(above/align "left" (text "(-) - zoom out" 10 "black")
             (text "press c to clear this screen" 10 "blue"))))))))))))

(define help_screen (place-image help_text 250 250 (empty-scene 500 500)))
(define help-scene (above color-palette help_screen))
;DOT STRUCTURE
(define-struct dot (diameter opacity color x y scene))

(define current (make-dot 5 125 "black" 475 25 scene-1))
(define (colored-dot diameter opacity color) (circle diameter opacity color))
(define (change-color current new-color) (make-dot (dot-diameter current)
                                                   (dot-opacity current) new-color
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
                            (dot-color current)) (dot-x current) (dot-y current) (dot-scene current)))
;MOUSE CONTROL
(define (mouse-handle current x y mouse-event)
  (cond [(string=? mouse-event "button-up") 
         (cond [(<= y 50)
             (change-color current (get-pixel-color x y color-palette))] 
        [else (change-posn current 475 25)])]
        [(string=? mouse-event "button-down")
         (render-scene current (draw-on-scene (change-posn current x y)))]        
        [(string=? mouse-event "drag")
        (render-scene current (draw-on-scene (change-posn current x y)))]
        [else (render-scene current (dot-scene current))]))

;KEYBOARD CONTROL
(define (increase-diameter current)(make-dot (cond [(< (dot-diameter current) 25)
                                    (+ (dot-diameter current) 1)]
                                    [else (+ (dot-diameter current) 0)]) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current)))

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
        [(string=? key "h") (render-scene current (overlay help-scene (dot-scene current)))]
        [(string=? key "g") (render-scene current (overlay grid-scene (dot-scene current)))]
        [(string=? key "c") (render-scene current scene-1)]
        [(string=? key "s") (save-image (crop 0 50 500 450 (dot-scene current)) "racket drawing.png")]
        [(string=? key "add") (render-scene current (above/align "left" color-palette (scale 1.25 (crop 0 50 500 450 (dot-scene current)))))]
        [(string=? key "subtract") (render-scene current (above/align "left" color-palette (scale 0.75 (crop 0 50 500 450 (dot-scene current)))))]
        [else current]))

;PROGRAM FUNCTION
(define draw (big-bang (make-dot 10 150 "black" 475 25 help-scene)                               
            (on-draw draw-on-scene)
            (on-key key-handle)
            (on-mouse mouse-handle)))

