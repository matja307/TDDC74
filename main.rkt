(load "matrix_imm.rkt")
(load "make-board.rkt")
(load "object-system.scm")
(load "ui.rkt")

(define board (make-board))

(define (main-loop)
  (let ((move (get-next-move)))
    (when (not (eq? move 'aborted))
      (ask board 'put-brick! 1 (x move) (y move))
      (ask board 'print-matrix)
      (newline)(newline)
      (set-piece-at! (x move) (y move)
                     'BLACK)))
  (main-loop))

(thread main-loop)