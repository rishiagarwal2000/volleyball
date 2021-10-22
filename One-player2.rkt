#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(define-struct posn (x y) #:transparent)
(define width 1000)
(define height 600)

(define back (empty-scene width height "medium cyan"))
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

(define automatic-player%
  (class object%
    (super-new)
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
    (define/public (draw img)
      (place-image image (posn-x cen) (posn-y cen) img))
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
                 (begin (display "Calculated x : ")
                        (displayln x)
                        x)]
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
         (cond [(and (> (posn-x (get-field cen ball)) 500) (< (abs (- (posn-x (get-field cen ball)) (posn-x cen))) (* 2 rad)) (> (posn-y (get-field cen ball)) (- ground-level 200))) (set! ucs-tick 0)]
               [(< (- (posn-x cen) landing) 20) (set! rcs-tick 0)]
               [(> (posn-x cen) landing) (set! lcs-tick 0)])))
                
    ))

(define p1 (new player% [k1 "right"] [k2 "left"] [k3 "up"] [cen (make-posn 200 (- ground-level (/ rad 2)))] [color 'red]))
(define p2 (new automatic-player% [cen (make-posn 800 (- ground-level (/ rad 2)))] [color 'blue]))

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

(define (key-handler w k)
  (cond [(or (key=? k "up") (key=? k "left") (key=? k "right"))
         (begin (send p1 key-helper k) w)]
        [else w]))

(define (release-handler w k) 
  (cond [(or (key=? k "left") (key=? k "right") (key=? k "up"))
         (begin (send p1 release-helper k) w)]
        [else w]))

(define (draw-world w)
  (draw-p1 (get-field  p1 w) (draw-ball (get-field ball w) (draw-p1 (get-field p2 w) background))))
  
(define (draw-p1 player img)
  (send player draw img))

(define (draw-ball ball img)
  (send ball draw img))
  
(define (world-step w)
  (begin
    (cond [(send w collision-with-player? p1) (begin (send w collision-with-player p1) (set-field! landing p2 -1))]
          [(send w collision-with-player? p2) (begin (display "Original x  : ") (displayln (posn-x (get-field cen ball))) (send w collision-with-player p2) (set-field! landing p2 -1))]
          [(send w collision-with-left-wall?) (send ball change-vx-left)]
          [(send w collision-with-right-wall?) (send ball change-vx-right)]
          [(send w collision-with-ceiling?) (send ball change-vy-ceiling)]
          [(send w collision-with-left-floor?) (begin (send ball reset-right) (send p1 reset) (send p2 reset) (set-field! par-tick p2 0))]
          [(send w collision-with-right-floor?) (begin (display "Original x  : ") (displayln (posn-x (get-field cen ball))) (send ball reset-left) (send p1 reset) (send p2 reset))]
          [(send w collision-p1-left-wall?) (send p1 change-vx-left)]
          [(send w collision-p1-net?) (send p1 change-vx-right)]
          [(send w collision-p2-net?) (send p2 change-vx-left)]
          [(send w collision-p2-right-wall?) (send p2 change-vx-right)]
          [(send w collision-ball-net-side?) (send ball collision-net-side)]
          [(send w collision-ball-net-top?) (send ball collision-net-top)])
    (send w clock-tick))
  w)

(define ball (make-object ball% (make-posn 200 340)))
(define w-init (make-object world% p1 ball p2))

(big-bang w-init
  (to-draw draw-world)
  (on-tick world-step DELTA)
  (on-key key-handler)
  (on-release release-handler))



          
                          
         
                

            
                        
           
          