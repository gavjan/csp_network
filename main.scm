(include "queue.scm")
(define q (make-queue))

(q 'add 1)
(display (q 'pop)) (newline)
(display (q 'empty?)) (newline)

