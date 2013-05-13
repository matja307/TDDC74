(load "object-system.scm")
(load "init.rkt")
(load "make-board.rkt")
(load "player.rkt")

(init)

;; proc: wait 
;; param: (int) ms
;; return: (void)
;; Pauses program in ms milliseconds. 
(define (wait ms)
  (let ((go #f)) 
    (define timer
      (new timer% [notify-callback (lambda () (set! go #t))]))
    (define (rec)
      (cond ((not go) (rec))))
    (send timer start ms)
    (rec)))

;; proc: get-next-move 
;; param: (player) player
;; return: (pair) (x . y)
;; Returns the next move from acive player
(define (get-next-move player)
  (if (ask player 'ai?)
      (begin
        (wait 700)
        (ask player 'calculate-move))
      (begin
        (get-next-move))))

;; proc: swich-turn
;; param: -
;; return: (void)
;; Swiches player turn. 
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
         (player-color (ask player 'color)))
    
    (cond ((and (or (null? (ask *board* 'valid-moves-list 0)) (= (ask (car *player-list*) 'bricks) 0))
                (or (null? (ask *board* 'valid-moves-list 1)) (= (ask (cdr *player-list*) 'bricks) 0)))
           (display "Game Over!")(newline))
          ((or (null? (ask *board* 'valid-moves-list player-color)) (= (ask player 'bricks) 0))
             (swich-turn)
             (main-loop))
          (else
           (begin
             (let ((move (get-next-move player)))
               (when (and (not (eq? move 'aborted)) (ask *board* 'valid-move? (x move) (y move) player-color)) 
                 
                 (ask player 'countdown-bricks)
                 (ask *board* 'put-brick! (x move) (y move) player-color)
                 (ask *board* 'flip! (x move) (y move) player-color)
                 
                 (if (= 0 player-color)
                     (set-piece-at! (x move) (y move) 'BLACK)
                     (set-piece-at! (x move) (y move) 'WHITE))
                 
                 (swich-turn))
               (main-loop)))))))

(thread main-loop)