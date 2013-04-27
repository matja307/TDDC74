(load "make-board.rkt")
(load "object-system.scm")
(load "ui.rkt")

(define *board* (make-board))
(define *player-list* (cons 0 1))
(define *turn* car)


(define (main-loop)
  (let ((move (get-next-move))
        (player-int (*turn* *player-list*)))
    (when (not (eq? move 'aborted))
      (ask *board* 'put-brick! (x move) (y move) player-int)
      
      (if (= 0 player-int)
          (set-piece-at! (x move) (y move) 'BLACK)
          (set-piece-at! (x move) (y move) 'WHITE))
      
      (if (eq? *turn* car)
          (set! *turn* cdr)
          (set! *turn* car))))
  (main-loop))

(thread main-loop)