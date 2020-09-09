#lang racket

(provide fill-abilities)
(provide compose-abilities)
(provide hourglass)

(provide get-ability-image)
(provide get-ability-time)
(provide get-ability-pos)
(provide get-ability-next)
(provide get-ability-active)

(provide set-ability-image)
(provide set-ability-time)
(provide set-ability-pos)
(provide set-ability-next)
(provide set-ability-active)

(provide my-ability)

(require "random.rkt")
(require lang/posn)
(require 2htdp/image)

; Imaginea si range-ul în care vor aparea abilitațile
; Nu modificați
(define POSITION_RANGE '((300 2000) (30 550)))
(define (hourglass color) (underlay
 (rectangle 40 40 "solid" color)
 (polygon
  (list (make-posn 0 0)
        (make-posn 25 0)
        (make-posn 0 25)
        (make-posn 25 25))
  "outline"
  (make-pen "darkslategray" 5 "solid" "round" "round"))))


(define-struct my-ability
  (image time pos next active)
  #:transparent)

; Fiecare funcție returneaza o componenta a unei abilități.

; Ability getters
(define (get-ability-image   ability) (my-ability-image   ability))
(define (get-ability-time    ability) (my-ability-time    ability))
(define (get-ability-pos     ability) (my-ability-pos     ability))
(define (get-ability-next    ability) (my-ability-next    ability))
(define (get-ability-active  ability) (my-ability-active  ability))

; Ability setters
(define (set-ability-image ability new-image)
  (struct-copy my-ability ability [image new-image]))
(define (set-ability-time  ability new-time)
  (struct-copy my-ability ability [time new-time]))
(define (set-ability-pos ability new-pos)
  (struct-copy my-ability ability [pos (make-posn new-pos (posn-y (get-ability-pos ability)))]))
(define (set-ability-next ability new-next)
  (struct-copy my-ability ability [next new-next]))
(define (set-ability-active ability new-act)
  (struct-copy my-ability ability [active new-act]))

; Random position in POSITION_RANGE
(define (random-position range)
	(apply make-posn (map ((curry apply) random) range)))

; List of n random elements from L list
(define (choice-abilities n L)
	(sample (discrete-dist L) n))

; Give random pos to all abilities that don't have one yet.
(define (set-ability-rand-pos ability)
  (struct-copy my-ability ability [pos (random-position POSITION_RANGE)]))

(define (position-abilities abilities)
  (map (λ (ability)
         (if (null? (get-ability-pos ability))
              (set-ability-rand-pos ability)
              ability))
       abilities)) 

; Compose functions
(define (compose-abilities L)
  (cond [(null? L) (λ (x) x)]
        [(null? (cdr L)) (car L)]
        [else (compose (car L) (compose-abilities (cdr L)))]))

; Add to a initial list more abilities, until the list size is n
(define (fill-abilities initial n abilities)
  (append
   initial
   (position-abilities (choice-abilities (- n (length initial)) abilities))))
