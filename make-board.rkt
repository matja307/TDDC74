(load "matrix.rkt")
(load "object-system.scm")

(define (make-board)
  
  (define matrix (make-matrix))
    
  (define (put-brick! self x y player)
    (ask matrix 'set-cell! x y player))
  
  (define (valid-move? self x y player)
    #t)
  
  (define (count-bricks self)
    (ask matrix 'count-bricks 1))
  
  (define (print-matrix self)
    (ask matrix 'print))

  
  (define (self msg)
    (cond
      ((eq? msg 'get-matrix) (lambda (self) matrix))
      ((eq? msg 'put-brick!) put-brick!)
      ((eq? msg 'valid-move?) valid-move?)
      ((eq? msg 'count-bricks) count-bricks)
      ((eq? msg 'print-matrix) print-matrix))) 
  
  self)