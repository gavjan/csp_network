(import (except (rnrs) define-record-type) (srfi :9))



(define (del-list-val x ls)
    (if (null? ls)
        '()
        (let ((h (car ls)))
            (
                (if (eqv? x h)
                    (lambda (y) y)
                    (lambda (y) (cons h y))
                )
                (del-list-val x (cdr ls))
            )
        )
    )
)

(define (inv-cons-name s)
    (if (string-prefix? "inv_" s)
        (substring s 4)
        (string-append "inv_" s)
    )
)

(define (inv-comp comp)
    (cond
        ((equal? comp <) >)
        ((equal? comp >) <)
        (else comp)
    )
)
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