(import (except (rnrs) define-record-type) (srfi :9))


(define (== left right)
    (equal? left right)    
)
(define (!= left right)
    (not (== left right))
)
(define (and-list lst) (or (equal? lst '()) (and (car lst) (and-list (cdr lst)))))
(define-record-type :constraint
    (make-constraint name comp left right)
    constraint?
    (name constraint-name set-constraint-name!)
    (comp constraint-comp set-constraint-comp!)
    (left constraint-left set-constraint-left!)
    (right constraint-right set-constraint-right!)
)

(define-record-type :cell
    (make-cell curr vals)
    cell?
    (curr cell-curr set-cell-curr!)
    (vals cell-vals set-cell-vals!)
)
