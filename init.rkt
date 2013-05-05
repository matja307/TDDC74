(require (file "othello-ui.scm"))

(define *board* '())
(define *player-list* '())
(define *turn* '())

(define (init)
  (clear-board!)
  
  (set! *board* (make-board))
  (set! *player-list* (cons (make-player 0 #f) (make-player 1 #t)))
  (set! *turn* car)
  
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
;  (ask (car *player-list*) 'countdown-bricks)
  
  
  (ask *board* 'put-brick! 3 3 1)
  (ask *board* 'put-brick! 4 3 0)
  (ask *board* 'put-brick! 3 4 0)
  (ask *board* 'put-brick! 4 4 1)
  (set-piece-at! 3 3 'WHITE)
  (set-piece-at! 4 3 'BLACK)
  (set-piece-at! 3 4 'BLACK)
  (set-piece-at! 4 4 'WHITE))

(set-quit-fn! hide-board)
(set-restart-fn! init)

(define x car)
(define y cdr)

(show-board)

