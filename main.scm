(include "network.scm")

(define (test-marx)
    (define solutions '())
    (define marx (create-network))

    (build-cell "pianista"     marx (list "grucho" "harpo" "chico"))
    (build-cell "harfiarz"     marx (list "grucho" "harpo" "chico"))
    (build-cell "gadula"       marx (list "grucho" "harpo" "chico"))
    (build-cell "pieniadze"    marx (list "grucho" "harpo" "chico"))
    (build-cell "hazard"       marx (list "grucho" "harpo" "chico"))
    (build-cell "zwierzeta"    marx (list "grucho" "harpo" "chico"))

    ;; 1) Pianista, harfiarz i gadula to różne osoby.
    (build-constraint "pianista/harfiarz" marx != "pianista" "harfiarz")
    (build-constraint "harfiarz/gadula" marx != "harfiarz" "gadula")
    (build-constraint "pianista/gadula" marx != "pianista" "gadula")

    ;; 2) Ten kto lubi pieniądze nie jest tym, kto lubi hazard, a ten z kolei nie lubi zwierząt
    (build-constraint "pieniadze/hazard" marx != "pieniadze" "hazard")
    (build-constraint "hazard/zwierzeta" marx != "hazard" "zwierzeta")

    ;; 3) Gaduła nie lubi hazardu.
    (build-constraint "gadula/hazard" marx != "gadula" "hazard")

    ;; 4) Harfiarz lubi zwierzęta.
    (build-constraint "harfiarz==zwierzeta" marx == "harfiarz" "zwierzeta")

    ;; 5) Groucho nie lubi zwierząt.
    (build-constraint "grucho!=zwierzeta" marx != "grucho" "zwierzeta")

    ;; 6) Harpo nigdy nic nie mówi.
    (build-constraint "harpo!=gadula" marx != "harpo" "gadula")

    ;; 7) Chico gra na pianinie.
    (build-constraint "chico==pianista" marx == "chico" "pianista")

    (set! solutions (run-csp marx #t))
    (display (car solutions)) (newline)
    (display (cadr solutions)) (newline)
)

(define (test-nums)
    (define nums (create-network))
    (build-cell "A" nums (list 1 2 3))
    (build-cell "B" nums (list 1 2 3))
    (build-cell "C" nums (list 1 2 3))

    (build-constraint "A>B" nums > "A" "B")
    (build-constraint "B=C" nums == "B" "C")
    (display (run-csp nums)) (newline)
)
(test-marx)
(test-nums)