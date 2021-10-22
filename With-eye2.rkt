#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;(define-struct posn (x y) #:transparent)
(define width 1000)
(define height 600)
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
                                              ) (list (make-posn 500 180)
                                                      (make-posn 500 220)
                                                      ) img)]
          [(= p2-score 6) (place-images (list (text "Player 2 Wins" 36 "indigo")
                                              (text "Press space to restart" 18 "black")
                                              ) (list (make-posn 500 180)
                                                      (make-posn 500 240)
                                                      ) img)])))


(define (master-reset k)
  (cond [(key=? k " ") (begin (send ball reset-left)
                              (send p1 reset)
                              (send p2 reset)
                              (set! p1-score 0)
                              (set! p2-score 0))]))
        
                              
(define back (empty-scene width height "cyan"))
(define ground-level 500)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (field [cen2 (make-posn 0 0)])
    (field [image 'undefined])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
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
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (define/public (draw img)
      (let* ([x-ball (posn-x (get-field cen ball))]
             [y-ball (posn-y (get-field cen ball))]
             [x1 (+ (posn-x cen) (posn-x cen2))]
             [y1 (+ (posn-y cen) (posn-y cen2))] ;cen2 is the offset of center of white eye from center of player
             [x2 (- x-ball x1)]
             [y2 (- y1 y-ball)]
             [dis (sqrt (+ (* x2 x2) (* y2 y2)))]
             [cos (/ x2 dis)]
             [sin (/ y2 dis)])
        (place-image (underlay/offset image (+ (posn-x cen2) (* 8 cos)) (- (posn-y cen2) (* 8 sin)) (circle 5 'solid 'black)) (posn-x cen) (posn-y cen) img)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
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

(define automatic-player%
  (class object%
    (super-new)
    (init-field cen)
    (init-field color)
    (define default-cen cen)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (field [cen2 (make-posn 0 0)])
    (field [image 'undefined])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
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
      (set! left-click? #f)
      (set! landing 0)
      (set! rcs-tick -1)
      (set! par-tick -1)
      (set! lcs-tick -1)
      (set! ucs-tick -1))
    (define/public (change-vx-left)
      (begin (set! left-click? #f) (set! vx 0)))
    (define/public (change-vx-right)
      (begin (set! right-click? #f) (set! vx 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (define/public (draw img)
      (let* ([x-ball (posn-x (get-field cen ball))]
             [y-ball (posn-y (get-field cen ball))]
             [x1 (+ (posn-x cen) (posn-x cen2))]
             [y1 (+ (posn-y cen) (posn-y cen2))] ;cen2 is the offset of center of white eye from center of player
             [x2 (- x-ball x1)]
             [y2 (- y1 y-ball)]
             [dis (sqrt (+ (* x2 x2) (* y2 y2)))]
             [cos (/ x2 dis)]
             [sin (/ y2 dis)])
        (place-image (underlay/offset image (+ (posn-x cen2) (* 8 cos)) (- (posn-y cen2) (* 8 sin)) (circle 5 'solid 'black)) (posn-x cen) (posn-y cen) img)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (define/public (simulate-landing-position
                    x-ball
                    y-ball
                    vx-ball
                    vy-ball
                    g-ball)
      (define x x-ball)
      (define y y-ball)
      (define vx vx-ball)
      (define vy vy-ball)
      (define g g-ball)
      (define (helper)
        (begin
          (cond [(<= x ball-rad) (cond [(< vx 0)
                                        (set! vx (/ (- vx) 2))])]
                [(>= x (- width ball-rad))
                 (cond [(> vx 0) (set! vx (/ (* -1 vx) 2))])]
                [(< y ball-rad)
                 (cond [(> vy 0) (set! vy (/ (* -1 vy) 2))])]
                [(and (<= x (+ 505 ball-rad)) (>= x (- 495 ball-rad)) (> y (- ground-level 80 ball-rad)))
                 (set! vx (* -1 vx))]
                [(and (<= x 505) (>= x 495) (> y (- ground-level 80 ball-rad)))
                 (set! vy (* -1 vy))])
          (set! x (+ x (* vx DELTA)))
          (set! y (+ (- y (* vy DELTA)) (/ (* g DELTA DELTA) 2)))
          (set! vy (- vy (* g DELTA)))
          (cond [(< vy 0) (set! g 1000)]
                [(> vy 0) (set! g 300)])
          (cond [(>= y (- ground-level ball-rad))
                 x]
                [else (helper)])))
      (helper))
    (define/public (tick-tock)
      (begin (set! cen (make-posn (+ (posn-x cen) (* vx DELTA)) (- (posn-y cen) (* vy DELTA))))
             (cond [(and (not (= vy 0)))
                    (cond [(= (posn-y cen) (- ground-level (/ rad 2))) (begin (set! vy 0) (set! cen (make-posn (posn-x cen) (- ground-level (/ rad 2)))))]
                          [(equal? (posn-y cen) (- ground-level (/ rad 2) (* 38 (* vy DELTA)))) (set! vy (- vy))])])
             (cond [(> landing 500) (update-state-variables)])
             (calc-landing)
             (play-after-reset)
             (right-click-simulator)
             (left-click-simulator)
             (up-click-simulator)))
    (field [landing 0]) ;do nothing related to landing when its 0
    
    (field [par-tick -1]) ;used in PlayAfterReset
    (define rcs-tick -1) ;used in RightClickSimulator
    (define lcs-tick -1) ;used in LeftClickSimulator
    (define ucs-tick -1) ;used in UpClickSimulator
    
    (define (play-after-reset)
      (cond [(= par-tick -1) (void)]
            [(= par-tick 20) ;till 20 ticks it will go right and then take jump
             (begin (cond [right-click? (set! vx (- vx v0))])
                    (set! right-click? #f)
                    (and (not up-click?) (equal? (posn-y cen) (- ground-level (/ rad 2))))
                    (begin (set! vy 400)
                           (set! up-click? #t))
                    (set! par-tick (+ 1 par-tick)))]
            [(= par-tick 21) ;jump has been taken now reset
             (begin (set! up-click? #f)
                    (set! par-tick -1))]
            [else (begin
                    (cond [(not right-click?) (set! vx (+ vx v0))])
                    (set! right-click? #t)
                    (set! par-tick (+ par-tick 1)))]))
    
    (define (right-click-simulator)
      (cond [(= rcs-tick -1) (void)]
            [(= rcs-tick 1) ;till 1 tick go right
             (begin (cond [right-click? (set! vx (- vx v0))])
                    (set! right-click? #f)
                    (set! rcs-tick -1))]
            [else (begin (cond [(not right-click?) (set! vx (+ vx v0))])
                         (set! right-click? #t)
                         (set! rcs-tick (+ rcs-tick 1)))]))
    (define (left-click-simulator)
      (cond [(= lcs-tick -1) (void)]
            [(= lcs-tick 1)
             (begin (cond [left-click? (set! vx (+ vx v0))])
                    (set! left-click? #f)
                    (set! lcs-tick -1))]
            [else (begin (cond [(not left-click?) (set! vx (- vx v0))])
                         (set! left-click? #t)
                         (set! lcs-tick (+ lcs-tick 1)))]))
    (define (up-click-simulator)
      (cond [(= ucs-tick -1) (void)]
            [(= ucs-tick 1)
             (begin (set! up-click? #f))]
            [else (cond [(and (not up-click?) (equal? (posn-y cen) (- ground-level (/ rad 2))))
                         (begin (set! vy 400)
                                (set! up-click? #t))])
                  (set! ucs-tick (+ ucs-tick 1))]))
    
    (define (calc-landing)
      (if (= landing -1)
          (set! landing (simulate-landing-position
                         (posn-x (get-field cen ball))
                         (posn-y (get-field cen ball))
                         (get-field vx ball)
                         (get-field vy ball)
                         (get-field g ball)))
          (void)))

    (define (update-state-variables)
      (if (and (> (- landing (posn-x cen)) -40) (< (- landing (posn-x cen)) -20))
          (void)
         (cond [(and (> (posn-x (get-field cen ball)) 500) (< (abs (- (posn-x (get-field cen ball)) (posn-x cen))) rad) (> (+ ball-rad (posn-y (get-field cen ball))) (- ground-level rad))) (set! ucs-tick 0)]
               [(< (- (posn-x cen) landing) 20) (set! rcs-tick 0)]
               [(> (posn-x cen) landing) (set! lcs-tick 0)])))
                
    ))

(define ball-rad 12)
(define ball%
  (class object%
    (super-new)
    (init-field cen)
    (field [vx 0])
    (field [vy 0])
    (field [g 0])
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
(define world%
  (class object%
    (super-new)
    (init-field p1)
    (init-field ball)
    (init-field p2)  
    (define/public (clock-tick)                                                         
      (cond [(and (< p1-score 6) (< p2-score 6)) (begin (send p1 tick-tock)
                                                        (send p2 tick-tock)
                                                        (send ball tick-tock))]))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
      (and (< x (/ width 2)) (>= y (- ground-level ball-rad))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (define/public (collision-with-right-floor?)
      (define y (posn-y (get-field cen ball)))
      (define x (posn-x (get-field cen ball)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
      (and (> x (/ width 2)) (>= y (- ground-level ball-rad))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
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

  
(define (draw-p1 player img)
  (send player draw img))

(define (draw-ball ball img)
  (send ball draw img))
  
(define (world-step-one-player w)
  (begin
    (cond [(send w collision-with-player? p1) (begin (send w collision-with-player p1) (set-field! landing p2 -1))]
          [(send w collision-with-player? p2) (begin (send w collision-with-player p2) (set-field! landing p2 -1))]
          [(send w collision-with-left-wall?) (send ball change-vx-left)]
          [(send w collision-with-right-wall?) (send ball change-vx-right)]
          [(send w collision-with-ceiling?) (send ball change-vy-ceiling)]
          [(send w collision-with-left-floor?) (begin (send ball reset-right) (send p1 reset) (send p2 reset) (set-field! par-tick p2 0) (set! p2-score (+ 1 p2-score)))]
          [(send w collision-with-right-floor?) (begin (send ball reset-left) (send p1 reset) (send p2 reset) (set! p1-score (+ 1 p1-score)))]
          [(send w collision-p1-left-wall?) (send p1 change-vx-left)]
          [(send w collision-p1-net?) (send p1 change-vx-right)]
          [(send w collision-p2-net?) (send p2 change-vx-left)]
          [(send w collision-p2-right-wall?) (send p2 change-vx-right)]
          [(send w collision-ball-net-top?) (send ball collision-net-top)]
          [(send w collision-ball-net-side?) (send ball collision-net-side)]
          )
    (send w clock-tick))
  w)

(define (world-step w)
  (begin
    (cond [(send w collision-with-player? p1) (send w collision-with-player p1)]
          [(send w collision-with-player? p2) (send w collision-with-player p2)]
          [(send w collision-with-left-wall?) (send ball change-vx-left)]
          [(send w collision-with-right-wall?) (send ball change-vx-right)]
          [(send w collision-with-ceiling?) (send ball change-vy-ceiling)]       
          [(send w collision-with-left-floor?) (begin (send ball reset-right) (send p1 reset) (send p2 reset)  (set! p2-score (+ 1 p2-score)))]
          [(send w collision-with-right-floor?) (begin (send ball reset-left) (send p1 reset) (send p2 reset)  (set! p1-score (+ 1 p1-score)))]
          [(send w collision-p1-left-wall?) (send p1 change-vx-left)]
          [(send w collision-p1-net?) (send p1 change-vx-right)]
          [(send w collision-p2-net?) (send p2 change-vx-left)]
          [(send w collision-p2-right-wall?) (send p2 change-vx-right)]
          [(send w collision-ball-net-top?) (send ball collision-net-top)]
          [(send w collision-ball-net-side?) (send ball collision-net-side)]
          )
    (send w clock-tick))
  w)

(define ball 'undefined)
(define p1 'undefined)
(define p2 'undefined)

(define key-handler 'undefined)
(define release-handler 'undefined)
(define msg-handler 'undefined)
(define w-init 'undefined)
(define parity 0)
(define (decide-LR)
  (if (equal? parity 'odd)
      (begin (set-field! k1 p1 "right") (set-field! k2 p1 "left") (set-field! k3 p1 "up")
             (set-field! k1 p2 "d") (set-field! k2 p2 "a") (set-field! k3 p2 "w"))
      (begin (set-field! k1 p2 "right") (set-field! k2 p2 "left") (set-field! k3 p2 "up")
             (set-field! k1 p1 "d") (set-field! k2 p1 "a") (set-field! k3 p1 "w"))))

(define (runner-one)
  (set! w-init (make-object world% p1 ball p2))
  (big-bang w-init
    (to-draw draw-world)
    (on-tick world-step-one-player DELTA)
    (on-key key-handler)
    (on-release release-handler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (name "Rac Volley")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ))

(define (runner-two)
  (set! w-init (make-object world% p1 ball p2))
  (big-bang w-init
    (to-draw draw-world)
    (on-tick world-step DELTA)
    (on-key key-handler)
    (on-release release-handler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (name "Rac Volley")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))


(define (runner-network)
  (set! w-init (make-object world% p1 ball p2))
  (big-bang w-init
    (to-draw draw-world)
    (on-tick world-step DELTA)
    (on-key key-handler)
    (on-release release-handler)
    (on-receive msg-handler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (name "Rac Volley")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (register "111.91.105.59")))
(define (main choice)
  (cond [(equal? choice 'one)
         (begin (set! p1 (new player% [k1 "right"] [k2 "left"] [k3 "up"] [cen (make-posn 200 (- ground-level (/ rad 2)))] [color 'red]))
                (set! p2 (new automatic-player% [cen (make-posn 800 (- ground-level (/ rad 2)))] [color 'blue]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ChangesMade;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (set! p1-score 0)
                (set! p2-score 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-field! image p1 (underlay/offset (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" (get-field color p1))) 35 -5  (circle 12 'solid 'white)))
(set-field! cen2 p1 (make-posn 35 -5))
(set-field! image p2 (underlay/offset (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" (get-field color p2))) -35 -5 (circle 12 'solid 'white)))
(set-field! cen2 p2 (make-posn -35 -5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                
                (set! ball (make-object ball% (make-posn 200 340)))               
                (set! key-handler (lambda (w k) (cond [(and (< p1-score 6) (< p2-score 6))      
                                                       (cond [(or (key=? k "up") (key=? k "left") (key=? k "right"))
                                                              (begin (send p1 key-helper k) w)]
                                                             [else w])]
                                                      [else (cond [(or (key=? " " k) (key=? "\b" k))      
                                                                   (begin (master-reset k) w)]
                                                                  [else w])])))
                
                (set! release-handler (lambda (w k) (cond [(or (key=? k "left") (key=? k "right") (key=? k "up"))
                                                           (begin (send p1 release-helper k) w)] [else w])))
                (runner-one))]
        [(equal? choice 'two)
         (begin (set! p1 (new player% [k1 "right"] [k2 "left"] [k3 "up"] [cen (make-posn 200 (- ground-level (/ rad 2)))] [color 'red]))
                (set! p2 (new player% [k1 "d"] [k2 "a"] [k3 "w"] [cen (make-posn 800 (- ground-level (/ rad 2)))] [color 'blue]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ChangesMade;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (set! p1-score 0)
                (set! p2-score 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-field! image p1 (underlay/offset (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" (get-field color p1))) 35 -5  (circle 12 'solid 'white)))
(set-field! cen2 p1 (make-posn 35 -5))
(set-field! image p2 (underlay/offset (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" (get-field color p2))) -35 -5 (circle 12 'solid 'white)))
(set-field! cen2 p2 (make-posn -35 -5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                (set! ball (make-object ball% (make-posn 200 340)))         
                (set! key-handler (lambda (w k) (cond [(and (< p1-score 6) (< p2-score 6))             
                                                       (cond [(or (key=? k "up") (key=? k "left") (key=? k "right"))
                                                              (begin (send p1 key-helper k) w)]
                                                             [(or (key=? k "w") (key=? k "a") (key=? k "d"))
                                                              (begin (send p2 key-helper k) w)]
                                                             [else w])]
                                                      [else (cond [(or (key=? " " k) (key=? "\b" k))      
                                                                   (begin (master-reset k) w)]
                                                                  [else w])])))                
                (set! release-handler (lambda (w k) (cond [(or (key=? k "left") (key=? k "right") (key=? k "up"))
                                                           (begin (send p1 release-helper k) w)]
                                                          [(or (key=? k "w") (key=? k "a") (key=? k "d"))
                                                           (begin (send p2 release-helper k) w)]
                                                          [else w])))
                (runner-two))]
        [(equal? choice 'net)
         (begin (set! p1 (new player% [k1 "pause"] [k2 "pause"] [k3 "pause"] [cen (make-posn 200 (- ground-level (/ rad 2)))] [color 'red]))
                (set! p2 (new player% [k1 "pause"] [k2 "pause"] [k3 "pause"][cen (make-posn 800 (- ground-level (/ rad 2)))] [color 'blue]))
                (set! p1-score 0)
                (set! p2-score 0)
                (set-field! image p1 (underlay/offset (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" (get-field color p1))) 35 -5  (circle 12 'solid 'white)))
                (set-field! cen2 p1 (make-posn 35 -5))
                (set-field! image p2 (underlay/offset (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" (get-field color p2))) -35 -5 (circle 12 'solid 'white)))
                (set-field! cen2 p2 (make-posn -35 -5))
                (set! ball (make-object ball% (make-posn 200 340)))
                (set! key-handler (lambda (w k) (cond [(key=? k "up") (make-package w (list 'key k 'key "w"))]
                                                      [(key=? k "left") (make-package w (list 'key k 'key "a"))]
                                                      [(key=? k "right") (make-package w (list 'key k 'key "d"))]
                                                      [else w])))
                (set! msg-handler (lambda (w s-expr) (cond [(and (not (equal? (car s-expr) 'start)) (= (length s-expr) 1))
                                                             (begin (set! parity (car s-expr)) (decide-LR) w)]
                                                           [(equal? (car s-expr) 'start)
                                                            (begin (set! p1 (new player% [k1 "pause"] [k2 "pause"] [k3 "pause"]
                                                                                         [cen (make-posn 200 (- ground-level (/ rad 2)))] [color 'red]))
                                                                   (set! p2 (new player% [k1 "pause"] [k2 "pause"] [k3 "pause"]
                                                                                         [cen (make-posn 800 (- ground-level (/ rad 2)))] [color 'blue]))
                                                                   (set! p1-score 0)
                                                                   (set! p2-score 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                   (set-field! image p1 (underlay/offset (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" (get-field color p1))) 35 -5  (circle 12 'solid 'white)))
                                                                   (set-field! cen2 p1 (make-posn 35 -5))
                                                                   (set-field! image p2 (underlay/offset (crop/align "center" "top" (* 2 rad) rad (circle rad "solid" (get-field color p2))) -35 -5 (circle 12 'solid 'white)))
                                                                   (set-field! cen2 p2 (make-posn -35 -5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                   
                                                                   (set! ball (make-object ball% (make-posn 200 340)))
                                                                   (set! w (make-object world% p1 ball p2))
                                                                   (decide-LR) w)]
                                                           [else (let ([k (cadr s-expr)]
                                                                       [x (if (equal? parity 'odd) p2 p1)]
                                                                       [y (if (equal? parity 'odd) p1 p2)])
                                                                   (cond [(equal? (car s-expr) 'key)
                                                                          (cond [(or (key=? k "w") (key=? k "a") (key=? k "d"))
                                                                                         (begin (send x key-helper k) w)]
                                                                                [else (begin (send y key-helper k) w)])] 
                                                                         [(equal? (car s-expr) 'release)
                                                                          (cond [(or (key=? k "w") (key=? k "a") (key=? k "d"))
                                                                                   (begin (send x release-helper k) w)]
                                                                                [else (begin (send y release-helper k) w)])]))])))
                (set! release-handler (lambda (w k) (cond [(key=? k "up") (make-package w (list 'release k 'release "w"))]
                                                          [(key=? k "left") (make-package w (list 'release k 'release "a"))]
                                                          [(key=? k "right") (make-package w (list 'release k 'release "d"))]
                                                          [else w])))
                (runner-network))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MainMenu;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define back-main-menu (freeze width height (scale 2.14 (bitmap/file "Screen.png"))))

(define r1 (rectangle 300 50 'solid "blue"))
(define t1 (text "Single player" 32 "white"))
(define button-single-player (overlay t1 r1))

(define r2 (rectangle 300 50 'solid "blue"))
(define t2 (text "Two player" 32 "white"))
(define button-two-player (overlay t2 r2))

(define r3 (rectangle 300 50 'solid "blue"))
(define t3 (text "Play over network" 32 "white"))
(define button-network-player (overlay t3 r3))

(define (background-main-menu w) (place-images (list button-single-player
                                                     button-two-player
                                                     button-network-player)
                                               (list (make-posn 500 120)
                                                     (make-posn 500 200)
                                                     (make-posn 500 280))
                                               back-main-menu))

(define (click-handler w x y event) 
  (begin (cond [(and (>= x 350) (<= x 650) (>= y 95) (<= y 145) (mouse=? "button-down" event)) (main 'one)]
               [(and (>= x 350) (<= x 650) (>= y 175) (<= y 225) (mouse=? "button-down" event)) (main 'two)]
               [(and (>= x 350) (<= x 650) (>= y 255) (<= y 305) (mouse=? "button-down" event)) (main 'net)])
         0))

(big-bang 0
  (to-draw background-main-menu)
  (on-mouse click-handler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Changes Made;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (name "Main menu")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  )