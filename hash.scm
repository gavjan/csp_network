(use-modules (srfi srfi-1)
             (srfi srfi-13))
(define (my-hash str size)
    (remainder (string-hash-ci str) size)
)
(define (my-assoc str alist)
    (find
        (lambda (pair)
            (string-ci=? str (car pair))
        )
        alist
    )
)


(define (make-hashmap)
    (define mp (make-hash-table))
    
    (define (put key val)
        (hashx-set! my-hash my-assoc mp key val)
    )

    (define (del key)
        (hashx-remove! my-hash my-assoc mp key)
    )
    
    (define (get key)
        (hashx-ref my-hash my-assoc mp key)
    )
    (define (map-func func)
        (hash-map->list func mp)
    )
    
    (define (in? key)
        (not (equal? #f (hashx-ref my-hash my-assoc mp key)))
    )

    (define* (func call key #:optional val)
        (cond
            ((equal? call 'put) (put key val))
            ((equal? call 'del) (del key))
            ((equal? call 'get) (get key))
            ((equal? call 'in?) (in? key))
            ((equal? call 'map) (map-func key))
        )
    )
    func
)
