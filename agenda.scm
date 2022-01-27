(include "queue.scm")
(include "hash.scm")


(define (make-agenda)
    (define q (make-queue))
    (define set (make-hashmap))

    
    (define (empty?)
        (q 'empty?)
    )

    (define (add item)
        (if (not (set 'in? item))
        	(begin
        		(set 'put item #t)
        		(q 'add item)
    		)
    	)
    )

    (define (pop)
        (define ret (q 'pop))
        (set 'del ret)
        ret
    )
    (define* (func call #:optional arg)
        (cond
            ((equal? call 'add) (add arg))
            ((equal? call 'pop) (pop))
            ((equal? call 'empty?) (empty?))
        )
    )
    func
)
