#lang scheme
(require picturing-programs)

;fixed drawing while help screen up

;;;<IMAGE DEFINITIONS>
(define draw-space (rectangle 500 500 "outline" "black"))
(define white-background (rectangle 500 500 "solid" "white"))
(define clear-background (rectangle 500 500 "outline" "white"))
(define black-background (rectangle 500 500 "solid" "black")) 

;grid
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

;palette
(define palette-1 (frame (rectangle 50 50 "solid" "red")))
(define palette-2 (frame (rectangle 50 50 "solid" "orange")))
(define palette-3 (frame (rectangle 50 50 "solid" "yellow")))
(define palette-4 (frame (rectangle 50 50 "solid" "green")))
(define palette-5 (frame (rectangle 50 50 "solid" "blue")))
(define palette-6 (frame (rectangle 50 50 "solid" "purple")))
(define palette-7 (frame (rectangle 50 50 "solid" "brown")))
(define palette-8 (frame (rectangle 50 50 "solid" "black")))
(define palette-9 (frame (rectangle 50 50 "solid" "white")))
(define palette-10 (overlay (circle 25 "outline" "black") (rectangle 50 50 "solid" "white")))

(define color-palette (beside (beside (beside (beside (beside palette-1 palette-2) 
                        (beside palette-3 palette-4)) 
                (beside palette-5 palette-6)) 
        (beside palette-7 palette-8))
  (beside palette-9 palette-10)))


(define scene-1 (above color-palette draw-space))

(define grid_screen (grid 50 50 cell))

;HELP SCREEN
(define help_text (above (text "Racket Paint by James Nakano" 20 "black")
                           (above (text "Controls:" 15 "black")                     
(above/align "left" (text "up/down arrow keys - change opacity." 10 "black")
(above/align "left" (text "left/right arrow keys - change brush diameter" 10 "black")
(above/align "left" (text "(g) - toggle grid" 10 "black")
(above/align "left" (text "(c) - clear screen" 10 "black")
(above/align "left" (text "(u) - undo" 10 "black")
(above/align "left" (text "(h) - toggle help screen" 10 "black")
(above/align "left" (text "(s) - save drawing to racket drawing.png" 10 "black")
(above/align "left" (text "(+) - zoom in" 10 "black")
(above/align "left" (text "(-) - zoom out" 10 "black")
(above/align "left" (text "(p) - pan" 10 "black")             
             (text "press h to close this screen" 10 "blue"))))))))))))))

(define help_screen (place-image help_text 250 250 white-background))
(define help-scene (above color-palette help_screen))
;;<STRUCTURE DEFINITIONS>
(define scene_1 (list draw-space draw-space 0))
(define-struct settings (help grid zoom pan))
(define-struct dot (diameter opacity color x y scene settings))

(define current (make-dot 10 150 "black" 475 25 scene_1 (make-settings "on" "off" 1 "off")))
(define (colored-dot diameter opacity color) (circle diameter opacity color))
(define (colored-square diameter opacity color) (rectangle diameter diameter opacity color))
(define (change-color current new-color) (make-dot (dot-diameter current)
                                                   (dot-opacity current)
                                                   new-color
                                                   (dot-x current)
                                                   (dot-y current)                                                  
                                                   (dot-scene current)
                                                   (dot-settings current)))
(define (change-posn current x y) (make-dot (dot-diameter current)
                                            (dot-opacity current)
                                            (dot-color current) 
                                            x y (dot-scene current)
                                            (dot-settings current)))


(define (render-scene current new-scene) (make-dot (dot-diameter current)
                                                   (dot-opacity current)
                                                   (dot-color current)
                                                    (dot-x current) (dot-y current)
                                                    new-scene
                                                    (dot-settings current)))
;;RENDERING DEFINTIONS                                                   
(define (draw-on-scene current)
  (cond [(and (string=? (settings-pan (dot-settings current)) "off")
              (string=? (settings-help (dot-settings current)) "off"))
(place-image (colored-dot (dot-diameter current)
                            (dot-opacity current)
                            (dot-color current)) (dot-x current) (dot-y current) (first (dot-scene current)))]
        [else (first (dot-scene current))]))
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
   (overlay/align "left" "top"
  (cond [(string=? (settings-help (dot-settings current)) "on")
                                       help_screen]
        [(string=? (settings-grid (dot-settings current)) "on")
                                       grid_screen]
        ;[(string=? (settings-pan (dot-settings current)) "on")
         ;
                                      [else draw-space])
        
  (draw-on-scene current))))

;MOUSE CONTROL
(define (mouse-handle current x y mouse-event)
  (cond [(string=? mouse-event "button-up") 
         (cond [(<= y 50)
             (change-color current (get-pixel-color x y color-palette))] 
        [else (change-posn current 475 25)])]
        [(and (string=? mouse-event "button-down") (> y 50))
         (cond [(string=? (settings-pan (dot-settings current)) "on")
                (render-scene current (cons (place-image (first (dot-scene current)) x y white-background) (dot-scene current)))]
         [else (render-scene current (cons (draw-on-scene (change-posn current x y)) (dot-scene current)))])]        
        [(and (string=? mouse-event "drag") (> y 50) (string=? (settings-pan (dot-settings current)) "off"))
        (cond [(string=? (settings-pan (dot-settings current)) "on")
                (render-scene current (cons (place-image (first (dot-scene current)) x y white-background) (dot-scene current)))]
         [else (render-scene current (cons (draw-on-scene (change-posn current x y)) (dot-scene current))) ])]
        [else current]))

;KEYBOARD CONTROL
(define (increase-diameter current)(make-dot (cond [(< (dot-diameter current) 25)
                                                    (+ (dot-diameter current) 1)]
                                                   [else (+ (dot-diameter current) 0)])  (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (dot-settings current)))

(define (decrease-diameter current) (make-dot (cond [(> (dot-diameter current) 1)
                                                     (- (dot-diameter current) 1)]
                                                    [else (- (dot-diameter current) 0)]) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (dot-settings current)))

(define (increase-opacity current) (make-dot (dot-diameter current) (cond [(< (dot-opacity current) 255)
                                                                           (+ (dot-opacity current) 1)]
                                                                          [else (+ (dot-opacity current) 0)]) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (dot-settings current)))

(define (decrease-opacity current) (make-dot (dot-diameter current) (cond [(> (dot-opacity current) 1)
                                                                           (- (dot-opacity current) 1)]
                                                                          [else (- (dot-opacity current) 0)]) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (dot-settings current)))

(define (toggle-help current) (if (string=? (settings-help (dot-settings current)) "on")
                                (make-dot (dot-diameter current) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (make-settings "off" (settings-grid (dot-settings current))
                                                                                                                                                        (settings-zoom (dot-settings current))
                                                                                                                                                        (settings-pan (dot-settings current))))
                                (make-dot (dot-diameter current) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (make-settings "on" (settings-grid (dot-settings current))
                                                                                                                                                        (settings-zoom (dot-settings current))
                                                                                                                                                        (settings-pan (dot-settings current))))))

(define (key-handle current key)
 (cond [(string=? key "left") (decrease-diameter current)]
        [(string=? key "right") (increase-diameter current)]
        [(string=? key "up") (increase-opacity current)]
        [(string=? key "down") (decrease-opacity current)]
        [(and (string=? key "u") (image? (first (rest (dot-scene current))))) (render-scene current (rest (dot-scene current)))]
        [(string=? key "h") (toggle-help current)]
        [(string=? key "g") (if (string=? (settings-grid (dot-settings current)) "off")
                                (make-dot (dot-diameter current) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (make-settings (settings-help (dot-settings current))
                                                                                                                                                                              "on"
                                                                                                                                                                              (settings-zoom (dot-settings current))
                                                                                                                                                                              (settings-pan (dot-settings current))))
                                (make-dot (dot-diameter current) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (make-settings (settings-help (dot-settings current))
                                                                                                                                                                              "off"
                                                                                                                                                                              (settings-zoom (dot-settings current))
                                                                                                                                                                              (settings-pan (dot-settings current)))))]
        [(string=? key "p") (if (string=? (settings-pan (dot-settings current)) "off")
                                (make-dot (dot-diameter current) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (make-settings (settings-help (dot-settings current))
                                                                                                                                                                              (settings-grid (dot-settings current)) 
                                                                                                                                                        (settings-zoom (dot-settings current))
                                                                                                                                                        "on"))
                                (make-dot (dot-diameter current) (dot-opacity current) (dot-color current) (dot-x current) (dot-y current) (dot-scene current) (make-settings (settings-help (dot-settings current))
                                                                                                                                                                              (settings-grid (dot-settings current))
                                                                                                                                                                              (settings-zoom (dot-settings current))
                                                                                                                                                                              "off")))]                                                                                                                                                
        [(string=? key "c") (render-scene current (cons scene-1 (dot-scene current)))]
        [(string=? key "s") (save-image (first (dot-scene current)) "racket drawing.png")]
        [(string=? key "add") (render-scene current (cons (scale 1.25 (first (dot-scene current))) (dot-scene current)))]
        [(string=? key "subtract") (render-scene current (cons (scale 0.75 (first (dot-scene current))) (dot-scene current)))]
        [else current]))

;PROGRAM FUNCTION
(define paint (big-bang current                              
            (on-draw draw_on_scene)
            (on-key key-handle)
            (on-mouse mouse-handle)))

