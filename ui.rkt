(require (file "othello-ui.scm"))

(set-quit-fn! hide-board)
(set-restart-fn! init)

(define x car)
(define y cdr)

(show-board)

