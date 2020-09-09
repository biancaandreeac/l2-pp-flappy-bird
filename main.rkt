#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "random.rkt")
(require "abilities.rkt")
(require "constants.rkt")

;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)

;---------------------------------------checker_exports------------------------------------------------
;; Initial state

; The bird structure
; x and y are the top left corner coordinates
; v-y the speed of falling/ possible momentum
(define-struct flappy-bird
  (x y v-y)
  #:transparent)

; A pipe contains 2 pipes actually, with a gap between them.
; The pipe structure
; x and y are the top left corner coordinates
(define-struct one-pipe
  (x y)
  #:transparent)

(define (make-pipe x)
  (one-pipe x (+ added-number (random random-threshold))))

(define slow-ability
  (my-ability
   (hourglass "tomato") ; image
   30        ; time
   null      ; pos
   (λ (var)
     (struct-copy my-variables var
          [scroll-speed (max 5 (- (get-variables-scroll-speed var) 1))])) ; next
   #f))      ; active

(define fast-ability
  (my-ability
   (hourglass "mediumseagreen") ; image
   10        ; time
   null      ; pos
   (λ (var)
     (struct-copy my-variables var
         [scroll-speed (+ (get-variables-scroll-speed var) 1)])) ; next
   #f))      ; active

; List with all possible abilities
(define ABILITIES (list slow-ability fast-ability))

; Variables structure
(define-struct my-variables
  (gravity momentum scroll-speed)
  #:transparent)

; Keep initial variables
(define initial-variables
  (my-variables initial-gravity initial-momentum initial-scroll-speed))

; State structure
(define-struct my-state
  (flappy-bird
   pipes
   score
   abilities
   variables)
  #:transparent)

; The initial state will have the bird with the speed 0, a pipe having a random height
; and the initial score, 0;
(define (get-initial-state)
  (my-state
   (flappy-bird bird-x bird-initial-y 0) ; bird
   (list (make-pipe scene-width))        ; pipes
   0                                     ; score
   (fill-abilities '() DISPLAYED_ABILITIES ABILITIES) ; abilities
   initial-variables))                   ; variables

; Bird getters
(define (get-bird state)
  (my-state-flappy-bird state))

(define (get-bird-y bird)
  (flappy-bird-y bird))

(define (get-bird-v-y bird)
  (flappy-bird-v-y bird))

; Gravitation logic
(define (next-state-bird bird gravity)
  (struct-copy flappy-bird bird
               ; x stays the same
               [y (+ (get-bird-y bird) (get-bird-v-y bird))] ; y increases with v-y
               [v-y (+ (get-bird-v-y bird) gravity)]))       ; the speed also increased

; Momentum logic
(define (next-state-bird-onspace bird momentum)
  (struct-copy flappy-bird bird
               [v-y (- 0 momentum)])) ; the speed gets the momentum value

;; Change
; Change it's responsible for the game input

; Manages the key controls
(define (change current-state pressed-key)
  (cond [(key=? pressed-key " ")                             ; if the key is SPACE, 
          (struct-copy my-state current-state                ; apply momentum
                      [flappy-bird (next-state-bird-onspace
                                     (get-bird current-state)
                                     initial-momentum)])]
        [else current-state]))                               ; any other key is ignored 


; Pipes getters
(define (get-pipes state)
  (my-state-pipes state))

(define(get-pipe-x pipe)
  (one-pipe-x pipe))

; Pipe moving logic
; Substracts the scroll-speed to evey pipe's x
(define (move-pipes pipes scroll-speed)
  (map (lambda (val-x a-pipe)
         (struct-copy one-pipe a-pipe [x val-x]))  ; "change" the value of x
       (map (match-lambda [(one-pipe x _)
                           (- x scroll-speed)]) pipes) pipes))
  
; When a pipe gets out of the screen, remove it
(define (clean-pipes pipes)
  (let ([first-pipe (car pipes)]) ; check the first one 
    (if (<= (+ (get-pipe-x first-pipe) pipe-width) 0)
        (cdr pipes)
        pipes)))

; If there are pipes removed, then we have to add others back
(define (add-more-pipes pipes)
  (if (>= (length pipes) no-pipes)
      pipes                          ; if there are enough pipes, we have nothing to do
      (append pipes
              (list (make-pipe (+ (get-pipe-x (car pipes))
                                  (* pipe-width (length pipes))
                                  (* pipe-gap (length pipes)))))))) 
      
; Use the pipe functions
; First we move the pipes, then we check if there are pipes that have to be removed
; and then add more pipes
(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

; Score getter
(define (get-score state)
  (my-state-score state))

; Check if the bird hits the ground
(define (check-ground-collision bird)
  (if (>= (+ (get-bird-y bird) bird-height) ground-y) #t #f))

; invalid-state?

; Check if the bird can still fly
(define (invalid-state? state)
  (or (check-ground-collision (get-bird state))
      (check-pipe-collisions (get-bird state)(get-pipes state))))

; Check if the bird hits a pipe
(define (check-pipe-collisions bird pipes)
  (not (null? (filter (λ (pipe)
                        (or (check-higher-pipe pipe bird)
                            (check-lower-pipe pipe bird))) pipes))))

; Check if it the bird has hit the upper pipe
(define (check-higher-pipe pipe bird)
  (check-collision-rectangles
   (make-posn bird-x (get-bird-y bird))                                 ; bird top left corner
   (make-posn (+ bird-x bird-width) (+ (get-bird-y bird) bird-height))  ; bird down right corner
   (make-posn (get-pipe-x pipe) 0)                                      ; pipe top left corner
   (make-posn (+ (get-pipe-x pipe) pipe-width) (one-pipe-y pipe))))     ; pipe down right corner

; Check if it the bird has hit the lower pipe
(define (check-lower-pipe pipe bird)
  (check-collision-rectangles
   (make-posn bird-x (get-bird-y bird))                                 ; bird top left corner
   (make-posn (+ bird-x bird-width) (+ (get-bird-y bird) bird-height))  ; bird down right corner
   (make-posn (get-pipe-x pipe) (+ (one-pipe-y pipe) pipe-self-gap))    ; pipe top left corner
   (make-posn (+ (get-pipe-x pipe) pipe-width) scene-height)))          ; pipe down right corner

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))


; Next-state
; Next-state will increment the score with 0.1 and it will also call the other next-state
; functions.
(define (next-state state)
  (struct-copy
   my-state state
   [flappy-bird (next-state-bird (get-bird state) initial-gravity)]   ; next state bird
   [pipes (next-state-pipes (get-pipes state)  initial-scroll-speed)] ; next state pipes
   [score (+ (get-score state) 0.1)]                                  ; next state score
   [abilities
    (next-abilities
     (get-abilities state) (get-bird state)
     (get-variables-scroll-speed (get-variables state)))]            ; next state abilities
   [variables
    (next-variables initial-variables
                    (get-abilities-active (get-abilities state)))])) ; next state variables
 

;; draw-frame
(define bird (rectangle bird-width bird-height  "solid" "yellow"))
(define ground (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

; Each frame will be displayed like this:
; bird over ground, over score, over pipes, over empty-scene.
; for the bonus I also added the abilities over everything.
(define (draw-frame state)
  (place-active-abilities
   (get-abilities-active (get-abilities state))
   (place-visible-abilities
    (get-abilities-visible (get-abilities state)) 
    (match-let ([(flappy-bird x y-bird _) (get-bird state)])
      (place-images
       ; list of shapes to be displayed
       (list bird ground (score-to-image (get-score state))) 
       ; list of each shape's coordinates
       (list
        (make-posn (+ x (quotient bird-width 2))
                   (+ y-bird (quotient bird-height 2)))      ; bird's coordinates
        (make-posn (quotient scene-width 2)                  ; ground's coordinates
                   (+ ground-y (quotient ground-height 2)))
        (make-posn text-x text-y))                           ; score's coordinates
       (place-pipes (get-pipes state) initial-scene))))))    ; pipes already placed

; Pipes placement
(define (place-pipes pipes scene)
  (foldl
   (lambda (pipe acc)
     (match-let ([(one-pipe x y) pipe])
       (place-images
        ; list of shapes to be displayed
        (list
         (rectangle pipe-width y "solid" "green")
         (rectangle pipe-width
                    (if (> (- scene-height (+ y pipe-self-gap)) 0)
                        (- scene-height (+ y pipe-self-gap)) 0) "solid" "green"))
        ; list of each shape's coordinates
        (list
         (make-posn (+ x (quotient pipe-width 2))
                    (quotient y 2))
         (make-posn (+ x (quotient pipe-width 2))
                    (+ (+ y pipe-self-gap)
                       (quotient (- scene-height (+ y pipe-self-gap)) 2))))
        acc))) scene pipes))

; Variables getters
(define (get-variables x)                 (my-state-variables x)) 
(define (get-variables-gravity var)       (my-variables-gravity      var))
(define (get-variables-momentum var)      (my-variables-momentum     var))
(define (get-variables-scroll-speed var)  (my-variables-scroll-speed var))


; Abilities getters.
(define (get-abilities x)
  (my-state-abilities x))

(define (get-abilities-visible x)
  (filter-not (λ (y) (get-ability-active y)) x))

(define (get-abilities-active x)
  (filter (λ (y) (get-ability-active y)) x))

; When a ability gets out of the screen, remove it
(define (clean-abilities abilities)
  (filter (λ (ability)
            (or
             (get-ability-active ability)
             (> (+ (posn-x (get-ability-pos ability)) 40) 0)))
          abilities))

; Ability moving logic
; Substracts the scroll-speed to evey ability's x
(define (move-abilities abilities scroll-speed)
  (map (λ (ability)
         (set-ability-pos ; this setter only modifies the x coordinate
          ability (- (posn-x (get-ability-pos ability)) scroll-speed)))
       abilities))

; When an ability is active, it has limited time on the screen. Substract a
; fraction of that time at every state.
(define (time-counter abilities)
  (filter (λ (ab)  ; ab is an individual ability.
            (> (get-ability-time ab) 0))
          (map (λ (ab)
                 (if (get-ability-active ab)
                     (set-ability-time ab (- (get-ability-time ab) (/ 1.0 fps)))
                     ab)) abilities)))

; Generate the next visible abilities. Makes sure that at each frame we have
; exactly a # of DISPLAYED_ABILITIES visible.
(define (next-abilities-visible visible scroll-speed)
  (let ([current-abilities (clean-abilities (move-abilities visible scroll-speed))])
    (if (< (length current-abilities) DISPLAYED_ABILITIES)
        (fill-abilities current-abilities DISPLAYED_ABILITIES ABILITIES)
        current-abilities)))

; Get the next abilities lot and check if another ability is now active.
(define (next-abilities abilities bird scroll-speed)
  (let ([visible  
         (next-abilities-visible 
          (filter-not (λ (x)
                        (get-ability-active x)) abilities) scroll-speed)])
    
    (map
     (λ(x) (if (check-active x bird) ; checks if an ability is activated
               (set-ability-active x #t) ; make ability active
               x)) ; otherwise just return the ability
         
         (append
          (time-counter (filter (λ (x) (get-ability-active x)) abilities))
          ; for the active abilities use the timer function
          visible)))); visible

; I also used the check-collision-rectangles function here.
(define (check-active ability bird)
  (check-collision-rectangles
   (make-posn bird-x (get-bird-y bird))                                ; bird top left corner
   (make-posn (+ bird-x bird-width) (+ (get-bird-y bird) bird-height)) ; bird down right corner
   
   (make-posn (- (posn-x (get-ability-pos ability)) 20)
              (- (posn-y (get-ability-pos ability)) 20))    ; ability top left corner
   (make-posn (+ (posn-x (get-ability-pos ability)) 20)     ; ability down right corner
              (+ (posn-y (get-ability-pos ability))  20))))

; Modify variables based on the active abilities
(define (next-variables variables abilities)
  ((compose-abilities (map (λ (ab) (get-ability-next ab)) abilities)) variables))


; Place the visible abilities
(define (place-visible-abilities abilities scene)
  (foldr
   (λ (ab acc)
     (place-image
      (get-ability-image ab)
      (posn-x (get-ability-pos ab))
      (posn-y (get-ability-pos ab))
      acc))
   scene abilities))


; Place the active abilities
(define (place-active-abilities abilities scene)
  (let loop ([i 0] [ab abilities])
    (if (null? ab)
        scene
        (place-image (scale 0.75 (get-ability-image (car ab)))
                     (- (posn-x abilities-posn) (* 50 i)) 
                     (posn-y abilities-posn)
                     (loop (add1 i) (cdr ab))))))

(module+ main
  (big-bang (get-initial-state)
    [on-tick next-state (/ 1.0 fps)]
    [to-draw draw-frame]
    [on-key change]
    [stop-when invalid-state?]
    [close-on-stop #t]
    [record? #f]))
