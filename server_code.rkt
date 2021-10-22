#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(define state-expr '())
(define no-of-worlds-connected 0)
(define iworld1 0)
(define iworld2 0)

(define (new-handler u iwor)
  (cond [(= u 0) (begin (displayln u) (printf "connected\n") (set! iworld1 iwor)
                                             (make-bundle 1 (list (make-mail iworld1 (list 'odd))) '()))]
        [(= u 1) (begin (displayln u) (printf "connected\n") (set! iworld2 iwor)
                                             (make-bundle 2 (list (make-mail iworld2 (list 'even))) '()))]
        [else (begin (printf "Not allowed") u)]))

(define (msg-handler u iwor s-expr)
  (cond [(iworld=? iwor iworld1) (if (iworld? iworld2) (make-bundle u (list (make-mail iworld2 (list-tail s-expr 2))
                                                      (make-mail iworld1 (drop-right s-expr 2))) '()) u)]
        [(iworld=? iwor iworld2) (if (iworld? iworld1) (make-bundle u (list (make-mail iworld1 (list-tail s-expr 2))
                                                      (make-mail iworld2 (drop-right s-expr 2))) '()) u)]
        [else (begin (printf "Error : invalid world") u)]))
(define (disc-handler u iw)
         (cond [(= u 2) (begin (displayln u) (printf "disconnected\n") (if (iworld=? iw iworld1) (begin (set! iworld1 iworld2) (set! iworld2 0)
                                                  (make-bundle 1 (list (make-mail iworld1 (list 'odd))
                                                                       (make-mail iworld1 (list 'start))) (list iw)))
                                          (begin (set! iworld2 0) (make-bundle 1 (list (make-mail iworld1 (list 'start))) (list iw)))))]
        [(= u 1) (begin (displayln u) (printf "disconnected\n")
                                             (set! iworld1 0) (make-bundle 0 '() (list iw)))]))
(universe 0
          (on-new new-handler)
          (on-msg msg-handler)
          (on-disconnect disc-handler))