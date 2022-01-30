(include "network.scm")

(define (test-marx)
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


    (display (marx 'run-csp)) (newline)
)

(define (test-nums)
    (define nums (create-network))
    (nums 'build-cell "A" (list 1 2 3))
    (nums 'build-cell "B" (list 1 2 3))
    (nums 'build-cell "C" (list 1 2 3))

    (nums 'build-constraint "A>B" > "A" "B")
    (nums 'build-constraint "B=C" == "B" "C")
    (display (nums 'run-csp #t)) (newline)
)
(test-nums)