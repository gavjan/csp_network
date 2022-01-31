(include "hash.scm")
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
                        func
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

    (define (set-cell-curr-val! name val)
        (define c (cells 'get name))
        (set-cell-curr! c val)
        
        (cells 'put name c)
    )

    (define (build-cell name vals network)
        (cells 'put name (make-cell name '() vals network))
    )

    (define (build-constraint name comp left right)
        (if
            (or
                (equal? comp >)
                (equal? comp <)
                (equal? comp ==)
                (equal? comp !=)
            )
            (constraints 'put name (make-constraint name comp left right func))
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
                (set-cell-curr-val! name (car l))
                (if (assert-constrains)
                    (run)
                )
                (run-for-vals name (cdr l))
                (set-cell-curr-val! name '())
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

    (define (pick for-cell val) 
        (define name (cell-name for-cell))
        (define c (cells 'get name))
        (set-cell-vals! c (list val))
        (cells 'put name c)
    )

    (define (exclude for-cell val)
        (define name (cell-name for-cell))
        (define c (cells 'get name))
        (set-cell-vals! c (del-list-val val (cell-vals c)))
        (cells 'put name c)
    )

    (define (update)
        (set! all_solutions? #f)
        (set! solutions '())
        (set! solved? #f)
    )

    (define (search)
        (run)
    )

    (define (what-are)
        (cells 'map
            (lambda (name c)
                (display name)
                (display ": ")
                (display (cell-vals c))
                (newline)
            )
        )
    )

    (define (determined?)
        (and-list
            (cells 'map
                (lambda (name c)
                    (len-1? (cell-vals c))
                )

            ) 
        )
    )

    (define (to-plunk)
        (del-list-val '() 
            (cells 'map
                (lambda (name c)
                    (if (len-1? (cell-vals c))
                        '()
                        name
                    )
                )
            )
        )
    )

    (define (what-is for-cell)
        (define name (cell-name for-cell))
        (define c (cells 'get name))
        (cell-vals c)
    )

    (define (known? for-cell)
        (define name (cell-name for-cell))
        (define c (cells 'get name))
        (len-1? (cell-vals c))
    )

    (define (value for-cell)
        (define name (cell-name for-cell))
        (define c (cells 'get name))
        (if (known? for-cell)
            (car (cell-vals c))
            #f
        )
    )

    (define* (func call #:optional arg1 arg2 arg3 arg4)
        (cond
            ((equal? call 'build-cell) (build-cell arg1 arg2 func))
            ((equal? call 'build-constraint) (build-constraint arg1 arg2 arg3 arg4))
            ((equal? call 'run-csp) (run-csp arg1))
            ((equal? call 'lookup-cell) (lookup-cell arg1))
            ((equal? call 'lookup-constraint) (lookup-constraint arg1))
            ((equal? call 'pick) (pick arg1 arg2))
            ((equal? call 'exclude) (exclude arg1 arg2))
            ((equal? call 'optimize) (optimize))
            ((equal? call 'update) (update))
            ((equal? call 'search) (search))
            ((equal? call 'what-are) (what-are))
            ((equal? call 'determined?) (determined?))
            ((equal? call 'to-plunk) (to-plunk))
            ((equal? call 'what-is) (what-is arg1))
            ((equal? call 'known?) (known? arg1))
            ((equal? call 'value) (value arg1))
            
        )
    )
    func
)

(define (build-cell name network vals)
    (network 'build-cell name vals)
)
(define (exclude c val)
    ((cell-network c) 'exclude c val)
)
(define (pick c val)
    ((cell-network c) 'pick c val)
)
(define (build-constraint name network description c1 c2)
    (network 'build-constraint name description c1 c2)
)
(define (update cons)
    ((constraint-network cons) 'update)
)
(define* (run-csp network #:optional all?)
    (network 'run-csp all?)
)
(define (check-constraints network)
    (network 'optimize)
    network
)
(define (search-network network)
    (network 'search)
)
(define (lookup-cell name network)
    (network 'lookup-cell name)
)
(define (lookup-constraint name network)
    (network 'lookup-constraint name)
)
(define (what-are network)
    (network 'what-are)
)
(define (determined? network)
    (network 'determined?)
)
(define (to-plunk network)
    (network 'to-plunk)
)
(define (what-is c)
    ((cell-network c) 'what-is c)
)
(define (known? c)
    ((cell-network c) 'known? c)
)
(define (value c)
    ((cell-network c) 'value c)
)
