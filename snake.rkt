(module snek racket/base
  (require "game.rkt")
  (require "queues.rkt")
  (require racket/class)
  (require racket/draw)
  (require racket/gui/base)
  (provide run)
  
  ; (run-until-close draw-fn update-fn char-fn)
  ; (draw-fn dc)
  ; (char-fn char-events)
  ; (update-fn)
  (define screen-width 600)
  (define screen-height 400)
  
  (define rect-width 20)
  (define rect-height 20)
  (define board-width (/ screen-width rect-width))
  (define board-height (/ screen-height rect-height))
  
  (define (draw-rectangle-of-color dc x y color)
    (send dc set-pen color 0 'solid)
    (send dc set-brush color 'solid)
    (send dc draw-rectangle
          (* rect-width x)
          (* rect-height y)
          rect-width rect-height))
  
  (define (display-segment seg dc)
    (draw-rectangle-of-color dc (car seg) (cdr seg) "black"))
  
  (define (display-fruit fruit dc)
    (draw-rectangle-of-color dc (car fruit) (cdr fruit) "green"))
  
  (define (outside-map? x y)
    (or (>= x board-width) (>= y board-height) (< x 0) (< y 0)))
  
  (define (on-snake? snake x y)
    (not (for-queue-terminating 
          snake 
          (lambda (segment)
            (not (and (= x (car segment))
                      (= y (cdr segment))))))))
  
  (define (count-snake snake)
    (let ([counter 0])
      (for-queue snake
                 (lambda (segment)
                   (set! counter (add1 counter))))
      counter))
  
  (define (on-fruit? fruit x y)
    (and (= x (car fruit)) (= y (cdr fruit))))
  
  (define (new-random-fruit)
    (cons (random (round board-width)) (random (round board-height))))
  
  (define (run)
    (let ([snake (make-queue (cons 5 5) (cons 5 6) (cons 5 7))]
          [fruit (new-random-fruit)]
          [yv 0]
          [xv 1]
          [paused #f])
      (run-until-close
       (lambda (dc)
         (when paused
           (send dc set-pen "black" 1 'solid)
           (send dc draw-text "Paused" (- (/ screen-width 2) 30) (- (/ screen-height 20) 10)))
         (for-queue snake (lambda (seg) (display-segment seg dc)))
         (display-fruit fruit dc))
       
       (lambda ()
         (if paused
             #t
             (let* ([snake-head (peek-queue snake)]
                    [future-x (+ (car snake-head) xv)]
                    [future-y (+ (cdr snake-head) yv)])
               (if (or (outside-map? (car snake-head) (cdr snake-head)) ; game over
                       (on-snake? snake future-x future-y))
                   #f ; signals game end
                   (begin
                     (if (on-fruit? fruit future-x future-y)
                         (set! fruit (new-random-fruit))
                         (dequeue snake))
                     (enqueue (cons future-x future-y) snake)
                 #t)))))
       
       (lambda (char-event)
         (let ([key (send char-event get-key-code)])
           (cond [(or (eq? 'right key) (eq? #\l key))
                  (set! xv 1) (set! yv 0)]
                 [(or (eq? 'up key) (eq? #\k key))
                  (set! xv 0) (set! yv -1)]
                 [(or (eq? 'left key) (eq? #\h key))
                  (set! xv -1) (set! yv 0)]
                 [(or (eq? 'down key) (eq? #\j key))
                  (set! xv 0) (set! yv 1)]
                 [(eq? key #\space) (set! paused (not paused))])))
       
       (lambda (dc toplevel-frame current-canvas)
         (send dc set-pen "black" 1 'solid)
         (send dc draw-text "Game over." (- (/ screen-width 2) 30) (- (/ screen-height 20) 10))
         (display "You scored: ")
         (display (- (count-snake snake) 3))
         (display " points, nice job!")
         (newline)
         (when (eq? (message-box "Game over!" "Play again?" #f '(yes-no)) 'yes)
           (send toplevel-frame show #f)
           (run)))
       screen-width
       screen-height
       .075)))
  (run))