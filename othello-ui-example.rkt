;; Created:         021112
;; Last modified::  2004-10-26,  14:03:32
;; Created by:      Tord Svensson (torsv@ida.liu.se)

;; Load the user interface.
(require (file "othello-ui.scm"))


;; When the user presses the quit button we want to hide the board.
(set-quit-fn! hide-board)

;; When the user presses the restart button we want to print 
;; "Restarting..." and clear the board.
(set-restart-fn! 
 (lambda () 
   (display "Restarting...")
   (newline)
   (clear-board!)))


;; Show the user interface.
(show-board)


(define get-x car)
(define get-y cdr)

(define *pieces* '(BLACK WHITE))
(define *should-loop* #t)

;; Returns a random piece.
(define (random-piece)
  (list-ref *pieces* (random (length *pieces*))))

;; Loop, until the user presses quit, and read in a move and
;; set the piece at that board position to a randomly choosen
;;  piece. Also print the coordinates of the move.
(define (put-piece-loop)
  (let ((move (get-next-move)))
    (display "got-move ") 
    (display move)
    (newline)
    (when (not (eq? move 'aborted))
      (set-piece-at! (get-x move) (get-y move)
                     (random-piece))))
  (put-piece-loop))


;; Start the loop.
(thread put-piece-loop)

  