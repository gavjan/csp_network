(include "agenda.scm")
(include "structs.scm")

(define (create-network)
    (define cells (make-hashmap))
    (define constraints (make-hashmap))
    (define all_solutions? #f)
    (define solutions '())
    (define solved? #f)

    (define (clean)
        (set! solutions '())
        (set! solved? #f)
    )

    (define (optimize)
        #t ;; TODO
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
                (equal? comp =)
                (equal? comp !=)
            )
            (constraints 'put name (make-constraint name comp left right))
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
    (define (run)
        (if (not (check-over?))
            (if not (solution?)
                (begin
                    #f ;; TODO
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

(define marx (create-network))
(marx 'build-cell "A" (list 1 2 3))
(marx 'build-cell "B" (list 1 2 3))
(marx 'set-cell-val! "A" 1)
(marx 'set-cell-val! "B" 2)
(display (marx 'run-csp )) (newline)

