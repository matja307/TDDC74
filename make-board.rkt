(define (make-board)
  
  (define matrix (empty-matrix))
    
  (define (put-brick! self player x y)
    (set! matrix (matrix-change-cell matrix x y player)))
  
  (define (valid-move? self player x y)
    #t)
  
  (define (count-bricks self)
    (matrix-count matrix 1))
  
  (define (print-matrix self)
    (matrix-display matrix))

  
  (define (self msg)
    (cond
      ((eq? msg 'get-matrix) (lambda (self) matrix))
      ((eq? msg 'put-brick!) put-brick!)
      ((eq? msg 'valid-move?) valid-move?)
      ((eq? msg 'count-bricks) count-bricks)
      ((eq? msg 'print-matrix) print-matrix))) 
  
  self)