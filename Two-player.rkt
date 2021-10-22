#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)    ;;;;;;;;;;;;;Change made;;;;;;;;;;;;;;;;

;(define-struct posn (x y) #:transparent)   ;;;;;;;;;;;;;Change made;;;;;;;;;;;;;;;;
(define width 1000)
(define height 600)

(define back (empty-scene width height "medium cyan"))
(define ground-level 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define p1-score 0)      
(define p2-score 0)

(define empty-circle                                                      
  (circle 12 'outline (make-pen "Black" 4 "solid" "round" "round")))      
(define solid-circle
  (circle 12 'solid 'yellow))

(define (draw-world w)
  (let [(img (place-images (append
                        (build-list p1-score (lambda (x) (overlay empty-circle solid-circle)))
                        (build-list (- 12 p1-score p2-score) (lambda (x) empty-circle))
                        (build-list p2-score (lambda (x) (overlay empty-circle solid-circle))))
                       (append (build-list 6 (lambda (x) (make-posn (* 40 (+ x 1)) 30)))
                               (build-list 6 (lambda (x) (make-posn (- width (* 40 (+ x 1))) 30))))
                       (draw-p1 (get-field  p1 w) (draw-ball (get-field ball w) (draw-p1 (get-field p2 w) background)))))]
    (cond [(and (< p1-score 6) (< p2-score 6)) img]
          [(= p1-score 6) (place-images (list (text "Player 1 Wins" 36 "indigo")
                                              (text "Press space to restart" 18 "black")
                                              (text "Press backspace to return to menu" 18 "black")) (list (make-posn 500 180)
                                                                                                           (make-posn 500 240)
                                                                                                           (make-posn 500 260)) img)]
          [(= p2-score 6) (place-images (list (text "Player 2 Wins" 36 "indigo")
                                              (text "Press space to restart" 18 "black")
                                              (text "Press backspace to return to menu" 18 "black")) (list (make-posn 500 180)
                                                                                                           (make-posn 500 240)
                                                                                                           (make-posn 500 260)) img)])))


(define (master-reset k)
  (cond [(key=? k " ") (begin (send ball reset-left)
                              (send p1 reset)
                              (send p2 reset)
                              (set! p1-score 0)
                              (set! p2-score 0))]
       ;Add menu page initialisation;
        ;[(key=? k "\b") ..... ])
        ))
                              


                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                            
(define net (rectangle 10 80 "solid" "white"))
(define background (underlay/align "left" "bottom" (place-image net 500 (- ground-level 40) back)
                                   (above/align "left"
                                                (rectangle width 25 "solid" "yellowgreen")
                                                (rectangle width 25 "solid" "olivedrab")
                                                (rectangle width 25 "solid" "darkolivegreen")
                                                (rectangle width 25 "solid" "darkgreen"))))

(define rad 60)
(define DELTA 0.01)
(define player%
  (class object%
    (super-new)
    (init-field k1)
    (init-field k2)
    (init-field k3)
    (init-field cen)
    (init-field color)
    (define default-cen cen)
    (define image (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" color)))
    (field [vx 0])
    (field [vy 0])
    (field [v0 200])
    (field [up-click? #f])
    (field [right-click? #f])
    (field [left-click? #f])
    (define/public (reset)
      (set! cen default-cen)
      (set! vx 0)
      (set! vy 0)
      (set! up-click? #f)
      (set! right-click? #f)
      (set! left-click? #f))
    (define/public (change-vx-left)
      (begin (set! left-click? #f) (set! vx 0)))
    (define/public (change-vx-right)
      (begin (set! right-click? #f) (set! vx 0)))
    (define/public (tick-tock)
      (begin (set! cen (make-posn (+ (posn-x cen) (* vx DELTA)) (- (posn-y cen) (* vy DELTA))))
             (cond [(and (not (= vy 0)))
                    (cond [(= (posn-y cen) (- ground-level (/ rad 2))) (begin (set! vy 0) (set! cen (make-posn (posn-x cen) (- ground-level (/ rad 2)))))]
                          [(equal? (posn-y cen) (- ground-level (/ rad 2) (* 38 (* vy DELTA)))) (set! vy (- vy))])])))
    (define/public (draw img)
      (place-image image (posn-x cen) (posn-y cen) img))
    (define/public (key-helper k)
      (cond [(key=? k k1) (begin
                            (cond [(not right-click?) (set! vx (+ vx v0))])
                            (set! right-click? #t))]
            [(key=? k k2) (begin
                            (cond [(not left-click?) (set! vx (- vx v0))])
                            (set! left-click? #t))]
            [(key=? k k3) (cond
                            [(and (not up-click?) (equal? (posn-y cen) (- ground-level (/ rad 2))))
                             (begin (set! vy 400)
                                    (set! up-click? #t))])]))
    (define/public (release-helper k)
      (cond [(key=? k k1) (begin
                            (cond [right-click? (set! vx (- vx v0))])
                            (set! right-click? #f))]
            [(key=? k k2) (begin
                            (cond [left-click? (set! vx (+ vx v0))])
                            (set! left-click? #f))]
            [(key=? k k3) (set! up-click? #f)]))))

(define p1 (new player% [k1 "right"] [k2 "left"] [k3 "up"] [cen (make-posn 200 (- ground-level (/ rad 2)))] [color 'red]))
(define p2 (new player% [k1 "d"] [k2 "a"] [k3 "w"] [cen (make-posn 800 (- ground-level (/ rad 2)))] [color 'blue]))
 
(define world%
  (class object%
    (super-new)
    (init-field p1)
    (init-field ball)
    (init-field p2)
    (define/public (clock-tick)
      (begin (send p1 tick-tock)
             (send p2 tick-tock)
             (send ball tick-tock)))
    (define/public (key-pressed-ball k)
      (send ball key-helper k))
    (define/public (key-released-ball k)
      (send ball release-helper k))
    (define/public (collision-with-player? p)
      (define delta-x (- (posn-x (get-field cen ball)) (posn-x (get-field cen p))))
      (define delta-y (- (- (posn-y (get-field cen ball)) (+ (posn-y (get-field cen p)) (/ rad 2)))))
      (define dis (sqrt (+ (* delta-x delta-x) (* delta-y delta-y))))
      (<= dis  (+ ball-rad rad)))
    (define/public (collision-with-left-wall?)
      (define x (posn-x (get-field cen ball)))
      (<= x ball-rad))
    (define/public (collision-with-right-wall?)
      (define x (posn-x (get-field cen ball)))
      (>= x (- width ball-rad)))
    (define/public (collision-p1-left-wall?)
      (define x (posn-x (get-field cen p1)))
      (define vx (get-field vx p1))
      (and (< vx 0) (< x rad)))
    (define/public (collision-p1-net?)
      (define x (posn-x (get-field cen p1)))
      (define vx (get-field vx p1))
      (or (and (> x (- 495 rad)) (> vx 0))))
    (define/public (collision-p2-net?)
      (define x (posn-x (get-field cen p2)))
      (define vx (get-field vx p2))
      (or  (and (< vx 0) (< x (+ rad 505)))))
    (define/public (collision-p2-right-wall?)
      (define x (posn-x (get-field cen p2)))
      (define vx (get-field vx p2))
      (or (and (> x (- width rad)) (> vx 0)) ))
    (define/public (collision-with-ceiling?)
      (define y (posn-y (get-field cen ball)))
      (< y ball-rad))
    (define/public (collision-with-left-floor?)
      (define y (posn-y (get-field cen ball)))
      (define x (posn-x (get-field cen ball)))
      (and (< x (- (/ width 2) ball-rad 5)) (>= y (- ground-level ball-rad))))
    (define/public (collision-with-right-floor?)
      (define y (posn-y (get-field cen ball)))
      (define x (posn-x (get-field cen ball)))
      (and (> x (+ (/ width 2) ball-rad 5)) (>= y (- ground-level ball-rad))))
    (define/public (collision-ball-net-side?)
      (define y (posn-y (get-field cen ball)))
      (define x (posn-x (get-field cen ball)))
      (and (<= x (+ 505 ball-rad)) (>= x (- 495 ball-rad)) (> y (- ground-level 80 ball-rad))))
    (define/public (collision-ball-net-top?)
      (define y (posn-y (get-field cen ball)))
      (define x (posn-x (get-field cen ball)))
      (and (<= x 505) (>= x 495) (> y (- ground-level 80 ball-rad))))
    (define/public (collision-with-player p)
      (define delta-x (- (posn-x (get-field cen ball)) (posn-x (get-field cen p))))
      (define delta-y (- (- (posn-y (get-field cen ball)) (+ (posn-y (get-field cen p)) (/ rad 2)))))
      (define dis (sqrt (+ (* delta-x delta-x) (* delta-y delta-y))))
      (displayln dis)
      (define vx (get-field vx p))
      (define vy (get-field vy p))
      (send ball evaluate-speed delta-x delta-y dis vx vy))))


(define ball-rad 12)
(define ball%
  (class object%
    (super-new)
    (init-field cen)
    (define vx 0)
    (define vy 0)
    (define g 1000)
    (define v0 100)
    (define/public (collision-net-side)
      (set! vx (* -1 vx)))
    (define/public (collision-net-top)
      (set! vy (* -1 vy)))
    (define/public (reset-left)
      (begin
        (set! cen (make-posn 200 340))
        (set! vx 0)
        (set! vy 0)
        (set! g 0)))
    (define/public (reset-right)
      (begin
        (set! cen (make-posn 800 340))
        (set! vx 0)
        (set! vy 0)
        (set! g 0)))
    (define ball-image (circle ball-rad 'solid "fuchsia"))
    (define/public (draw img)
      (place-image ball-image
                   (posn-x cen)
                   (posn-y cen)
                   img))
    (define/public (change-vx-left)
      (cond [(< vx 0) (set! vx (/ (- vx) 2))]))
    (define/public (change-vx-right)
      (cond [(> vx 0) (set! vx (/ (- vx) 2))]))
    (define/public (change-vy)
      (set! vy (- vy)))
    (define/public (change-vy-ceiling)
      (cond [(> vy 0) (set! vy (/ (- vy ) 2))]))
    (define/public (evaluate-speed delta-x delta-y dis ux uy)
      (define u1 (- (* 2 (+ (* ux (/ delta-x dis)) (* uy (/ delta-y dis)))) (* vx (/ delta-x dis)) (* vy (/ delta-y dis))))
      (define u2 (- (* vy (/ delta-x dis)) (* vx (/ delta-y dis))))
      (define vxn (- (* u1 (/ delta-x dis)) (* u2 (/ delta-y dis))))
      (define vyn (+ (* u1 (/ delta-y dis)) (* u2 (/ delta-x dis))))
      (begin
        (set! g 1000)
        (set! vx vxn)
        (set! vy vyn)))
    (define/public (tick-tock)
      (begin (set! cen (make-posn (+ (posn-x cen) (* vx DELTA)) (+ (- (posn-y cen) (* vy DELTA)) (/ (* g DELTA DELTA) 2))))
             (set! vy (- vy (* g DELTA)))
             (cond[(< vy 0) (set! g 1000)]
                  [(> vy 0) (set! g 300)])))))
    
    
(define (key-handler w k)
  (cond [(and (< p1-score 6) (< p2-score 6))             ;;;;;;;;;;;;;;;ChangeMade;;;;;;;;;;;;;;;
         (cond [(or (key=? k "up") (key=? k "left") (key=? k "right"))
                (begin (send p1 key-helper k) w)]
               [(or (key=? k "w") (key=? k "a") (key=? k "d"))
                (begin (send p2 key-helper k) w)]
               [else w])]
        [else (cond [(or (key=? " " k) (key=? "\b" k))       ;;;;;;;;;;;ChangeMade;;;;;;;;;;;;;;
                     (begin (master-reset k) w)]
                    [else w])]))         ;;;;;;;;;;;;;;;;;;Changemade';;;;;;;;;;;
                    
                    

(define (release-handler w k) 
  (cond [(or (key=? k "left") (key=? k "right") (key=? k "up"))
         (begin (send p1 release-helper k) w)]
        [(or (key=? k "w") (key=? k "a") (key=? k "d"))
         (begin (send p2 release-helper k) w)]
        [else w]))


  
(define (draw-p1 player img)
  (send player draw img))

(define (draw-ball ball img)
  (send ball draw img))
  
(define (world-step w)
  (begin
    (cond [(send w collision-with-player? p1) (send w collision-with-player p1)]
          [(send w collision-with-player? p2) (send w collision-with-player p2)]
          [(send w collision-with-left-wall?) (send ball change-vx-left)]
          [(send w collision-with-right-wall?) (send ball change-vx-right)]
          [(send w collision-with-ceiling?) (send ball change-vy-ceiling)]
          [(send w collision-with-left-floor?) (begin (send ball reset-right) (send p1 reset) (send p2 reset) (set! p2-score (+ 1 p2-score)))] ;;;;;;;;;Change made;;;;;;;
          [(send w collision-with-right-floor?) (begin (send ball reset-left) (send p1 reset) (send p2 reset) (set! p1-score (+ 1 p1-score)))] ;;;;;;;;;Change-made;;;;;;;
          [(send w collision-p1-left-wall?) (send p1 change-vx-left)]
          [(send w collision-p1-net?) (send p1 change-vx-right)]
          [(send w collision-p2-net?) (send p2 change-vx-left)]
          [(send w collision-p2-right-wall?) (send p2 change-vx-right)]
          [(send w collision-ball-net-top?) (send ball collision-net-top)]
          [(send w collision-ball-net-side?) (send ball collision-net-side)])
    (send w clock-tick))
  w)

(define ball (make-object ball% (make-posn 500 340)))
(define w-init (make-object world% p1 ball p2))

(big-bang w-init
  (to-draw draw-world)
  (on-tick world-step DELTA)
  (on-key key-handler)
  (on-release release-handler))