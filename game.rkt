(module game racket/base
  (require racket/gui/base)
  (require racket/class)
  (provide run-until-close)
  
  (define game-canvas%
    (class canvas%
      (init parent)
      (init-field
       width
       height
       [paint-function (lambda (dc) 0)]
       [char-function (lambda (char-event) 0)])
      (super-new [parent parent] [min-width width] [min-height height])
      (define/override (on-paint)
        (let ([dc (send this get-dc)])
          (paint-function dc)))
      (define/override (on-char char-event)
        (char-function char-event))))
  
  (define (run-until-close draw-fn update-fn char-fn end-fn (width 400) (height 300) (delay .01))
    (let* ([parent-window (new frame% [label "A program"])]
           [canvas (new game-canvas% 
                        [parent parent-window]
                        [paint-function draw-fn]
                        [char-function char-fn]
                        [width width]
                        [height height])])
      (send canvas focus)
      (send parent-window show #t)
      (with-method ([is-shown? (parent-window is-shown?)]
                    [refresh-screen (canvas refresh-now)])
        (let loop ()
          (when (is-shown?)
            (sleep/yield delay)
            (when (update-fn)
              (refresh-screen)
              (loop))))
        (end-fn (send canvas get-dc) parent-window canvas))))
  
  (define (test)
    (let ([x 200]
          [xv 0]
          [y 150]
          [yv 0]
          [ac 3])
      (run-until-close
       (lambda (dc)
         (send dc draw-text "+" x y))
       (lambda ()
         (set! x (+ x xv))
         (set! y (+ y yv))
         #t)
       (lambda (char-event)
         (case (send char-event get-key-code)
           [(#\l) (set! xv ac) (set! yv 0)]
           [(#\k) (set! yv (- ac)) (set! xv 0)]
           [(#\j) (set! yv ac) (set! xv 0)]
           [(#\h) (set! xv (- ac)) (set! yv 0)]
           [(#\space) (set! xv 0) (set! yv 0)]))
       (lambda ()
         (display "Game-over.") (newline))
       400
       300))))