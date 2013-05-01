(load "object-system.scm")
(load "init.rkt")
(load "make-board.rkt")
(load "player.rkt")

(init)

(define (wait ms)
  (let ((go #f)) 
    (define timer
      (new timer% [notify-callback (lambda () (set! go #t))]))
    (define (rec)
      (cond ((not go) (rec))))
    (send timer start ms)
    (rec)))
  
(define (get-next-move player)
  (if (ask player 'ai?)
      (begin
        (wait 700)
        (ask player 'calculate-move))
      (begin
        (get-next-move))))

(define (swich-turn)
  (if (eq? *turn* car)
          (set! *turn* cdr)
          (set! *turn* car)))

(define (main-loop)
  (display "Svart: ") 
    (display (car (ask *board* 'count-bricks)))
    (display " - ")
    (display "Vit: ")
    (display (cdr (ask *board* 'count-bricks)))(newline)
  
  (let* ((player (*turn* *player-list*))
        (player-color (ask (*turn* *player-list*) 'color))
        (move (get-next-move player)))
   
    (if (null? move)
        (begin
          (swich-turn)
          (main-loop))
        (begin
          (when (and (not (eq? move 'aborted)) (ask *board* 'valid-move? (x move) (y move) player-color)) 
            
            (ask *board* 'put-brick! (x move) (y move) player-color)
            (ask *board* 'flip! (x move) (y move) player-color)
            
            (if (= 0 player-color)
                (set-piece-at! (x move) (y move) 'BLACK)
                (set-piece-at! (x move) (y move) 'WHITE))
            
            (swich-turn))))
  (main-loop)))

(thread main-loop)