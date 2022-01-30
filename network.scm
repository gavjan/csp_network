(include "agenda.scm")
(include "structs.scm")

(define (create-network)
    (define cells (make-hashmap))
    (define constraints (make-hashmap))
    (define all_solutions? #f)
    (define solutions '())
    (define solved? #f)
    (define agenda (make-agenda))
    (define arcs (make-hashmap))

    (define (clean)
        (set! solutions '())
        (set! solved? #f)
    )

    (define (contradiction? l)
        (and-list l)
    )
    (define (init-optimize l)
        (if (not (equal? l '()))
            (let ()
                (define c (car l))
                (define inv
                    (make-constraint
                        (inv-cons-name (constraint-name c))
                        (inv-comp (constraint-comp c))
                        (constraint-right c)
                        (constraint-left c)
                    )
                )
                ;; c.left in cells and c.right in cells
                (if
                    (and
                        (cells 'in? (constraint-left c))
                        (cells 'in? (constraint-right c))
                    )
                    (begin
                        (constraints 'put (constraint-name inv) inv)
                        (agenda 'add (constraint-name c))
                        (agenda 'add (constraint-name inv))
                        (arcs 'put (constraint-right c) c)
                        (arcs 'put (constraint-right inv) inv)
                        (init-optimize (cdr l))                
                    )
                )
            )
        )
    )
    (define (right-optimize-loop cons left-val lst)
        (if (equal? lst '())
            #t
            (let ()
                (define right-val (car lst))

                (if
                    (evaluate
                        (constraint-name cons)
                        (constraint-comp cons)
                        left-val
                        right-val
                    )
                    #f
                    (right-optimize-loop cons left-val (cdr lst))
                )                
            )
        )
    )
    (define (left-optimize-loop cons left-cell right-cell l)
        (if (not (equal? l '()))
            (let ()
                (define remove-left? #t)
                (if
                    (right-optimize-loop
                        cons
                        (car l)
                        (cell-vals (cells 'get right-cell))
                    )
                    (let ()
                        (define c (cells 'get left-cell))
                        (define new-vals (del-list-val (car l) (cell-vals c)))
                        (set-cell-vals! c new-vals)
                        (cells 'put left-cell c)

                        (agenda 'add
                            (inv-cons-name
                                (constraint-name cons)
                            )
                        )
                    )
                )
                (left-optimize-loop cons left-cell right-cell (cdr l))
            )
        )
    )

    (define (optimize-loop)
        (if (not (agenda 'empty?))
            (let ()
                (define cons (constraints 'get (agenda 'pop)))
                (define left-cell (constraint-left cons))
                (define right-cell (constraint-right cons))
                (left-optimize-loop cons left-cell right-cell (cell-vals (cells 'get left-cell)))

                
                (optimize-loop)
            )
        )
    )
    (define (optimize)
        (set! agenda (make-agenda))
        (set! arcs (make-hashmap))


        (init-optimize
            (constraints 'map
                (lambda (name c)
                    c
                )
            )
        )

        (optimize-loop)

        (contradiction?
            (cells 'map
                (lambda (name c)
                    (not
                        (equal? (cell-vals c) '())
                    )
                )
            )
        )
    )

    (define (set-cell-val! name val)
        (define c (cells 'get name))
        (set-cell-curr! c val)
        
        (cells 'put name c)
    )

    (define (build-cell name vals)
        (cells 'put name (make-cell '() vals))
    )

    (define (build-constraint name comp left right)
        (if
            (or
                (equal? comp >)
                (equal? comp <)
                (equal? comp ==)
                (equal? comp !=)
            )
            (constraints 'put name (make-constraint name comp left right))
            (error name "unrecognized comperator for constraint" comp)
        )
    )

    (define (lookup-cell name)
        (cells 'get name)
    )
    
    (define (lookup-constraint name)
        (constraints 'get name)
    )
    (define (check-over?)
        (if solved?
            #t
            ;;solved? = (not all_solutions) and solutions != '()
            (begin
                (set! solved?
                    (and
                        (not all_solutions?)
                        (not (equal? solutions '() )) 
                    )
                )
                solved?
            )
        )
    )
    (define (save-solution)
        (define solution
            (cells 'map
                (lambda (name c)
                    (cons
                        name
                        (cell-curr c)
                    )
                )
            )
        )
        (if all_solutions?
            (if (not (member solution solutions))
                (set! solutions (cons solution solutions))
            )
            (set! solutions solution)
        )
        #t
    )
    (define (solution?)
        (if
            (and-list
                (cells 'map
                    (lambda (name c)
                        (not
                            (equal? (cell-curr c) '())
                        )
                    )
                )
            )
            (save-solution)
            #f
        )
    )
    (define (eval-expr name comp left right)
        (if
            (or
                (equal? comp >)
                (equal? comp <)
                (equal? comp ==)
                (equal? comp !=)
            )
            (comp left right)
            (error name "unrecognized comperator for constraint" comp)
        )

    )
    (define (evaluate name comp left right)     
        (if (and (string? left) (cells 'in? left))
            (set! left (cell-curr (cells 'get left)))
        )
        (if (and (string? right) (cells 'in? right))
            (set! right (cell-curr (cells 'get right)))
        )

        
        (if 
            (or
                (equal? left '())
                (equal? right '())
            )
            #t
            (eval-expr name comp left right)
        )
    )

    (define (assert-for-constraints l)   
        (if  (equal? l '())
            #t
            (if
                (evaluate
                    (constraint-name (car l))
                    (constraint-comp (car l))
                    (constraint-left (car l))
                    (constraint-right (car l))
                )
                (assert-for-constraints (cdr l))
                #f
            )
        )
    )

   
    (define (assert-constrains)
        (assert-for-constraints
            (constraints 'map
                (lambda (name c) c)
            )
        )

    )
    (define (run-for-vals name l)
        (if (not (equal? l '()))
            (begin
                (set-cell-val! name (car l))
                (if (assert-constrains)
                    (run)
                )
                (run-for-vals name (cdr l))
                (set-cell-val! name '())
            )
        )
    )

    (define (run-for-cells l)
        (if (not (equal? l '()))
            (let ()
                (define name (car (car l)))
                (define c (cdr (car l)))
                (if (equal? (cell-curr c) '())
                    (run-for-vals name (cell-vals c))
                )
                (run-for-cells (cdr l))
                
            )
        )
    )
    (define (run)
        (if (not (check-over?))
            (if (not (solution?))
                (run-for-cells
                    (cells 'map
                        (lambda (name c) (cons name c))
                    )
                )
            )
        )
    )

    (define (run-csp all?)
        (set! all_solutions? all?)
        (clean)

        (if (optimize)
            (run)
        )
        solutions
    )

    (define* (func call #:optional arg1 arg2 arg3 arg4)
        (cond
            ((equal? call 'build-cell) (build-cell arg1 arg2))
            ((equal? call 'build-constraint) (build-constraint arg1 arg2 arg3 arg4))
            ((equal? call 'run-csp) (run-csp arg1))
            ((equal? call 'lookup-cell) (lookup-cell arg1))
            ((equal? call 'lookup-constraint) (lookup-constraint arg1))
            ((equal? call 'set-cell-val!) (set-cell-val! arg1 arg2))
        )
    )
    func
)
