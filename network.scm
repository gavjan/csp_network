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
    (define (evaluate c)
        (define name (constraint-name c))
        (define comp (constraint-comp c))
        (define left (constraint-left c))
        (define right (constraint-right c))

        (if (cells 'in? left)
            (set! left (cell-curr (cells 'get left)))
        )
        (if (cells 'in? right)
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
            (if (evaluate (car l))
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

(define marx (create-network))
(marx 'build-cell "pianista"    (list "grucho" "harpo" "chico"))
(marx 'build-cell "harfiarz"    (list "grucho" "harpo" "chico"))
(marx 'build-cell "gadula"      (list "grucho" "harpo" "chico"))
(marx 'build-cell "pieniadze"   (list "grucho" "harpo" "chico"))
(marx 'build-cell "hazard"      (list "grucho" "harpo" "chico"))
(marx 'build-cell "zwierzeta"   (list "grucho" "harpo" "chico"))

;; 1) Pianista, harfiarz i gadula to różne osoby.
(marx 'build-constraint "pianista/harfiarz" != "pianista" "harfiarz")
(marx 'build-constraint "harfiarz/gadula" != "harfiarz" "gadula")
(marx 'build-constraint "pianista/gadula" != "pianista" "gadula")

;; 2) Ten kto lubi pieniądze nie jest tym, kto lubi hazard, a ten z kolei nie lubi zwierząt
(marx 'build-constraint "pieniadze/hazard" != "pieniadze" "hazard")
(marx 'build-constraint "hazard/zwierzeta" != "hazard" "zwierzeta")

;; 3) Gaduła nie lubi hazardu.
(marx 'build-constraint "gadula/hazard" != "gadula" "hazard")

;; 4) Harfiarz lubi zwierzęta.
(marx 'build-constraint "harfiarz==zwierzeta" == "harfiarz" "zwierzeta")

;; 5) Groucho nie lubi zwierząt.
(marx 'build-constraint "grucho!=zwierzeta" != "grucho" "zwierzeta")

;; 6) Harpo nigdy nic nie mówi.
(marx 'build-constraint "harpo!=gadula" != "harpo" "gadula")

;; 7) Chico gra na pianinie.
(marx 'build-constraint "chico==pianista" == "chico" "pianista")


(display (marx 'run-csp #t)) (newline)
