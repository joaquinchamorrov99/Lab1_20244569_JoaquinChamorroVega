#lang racket

(require "main_20244569_ChamorroVega.rkt")

;; ===================================================================
;; Permite que el script siga
;;         corriendo aunque ocurran errores en cualquier punto. Para probar initgame y shuffledeck




;; ACCIONES (para ataques, habilidades y entrenadores)

(define accion-noop (lambda (j . a) j))
(define accion-paralizar
  (lambda (j . a)
    (let* ((opp (game-opponent j)) (act (player-active opp)))
      (if (null? act) j
          (game-set-opponent j (player-set-active opp (ipc-set-status act "paralizado")))))))
(define accion-confundir
  (lambda (j . a)
    (let* ((opp (game-opponent j)) (act (player-active opp)))
      (if (null? act) j
          (game-set-opponent j (player-set-active opp (ipc-set-status act "confundido")))))))
(define accion-envenenar
  (lambda (j . a)
    (let* ((opp (game-opponent j)) (act (player-active opp)))
      (if (null? act) j
          (game-set-opponent j (player-set-active opp (ipc-set-status act "envenenado")))))))
(define accion-robar-1
  (lambda (j . a)
    (let* ((p (game-current-player j)) (d (player-deck p)))
      (if (deck-empty? d) j
          (let ((carta (deck-top d)) (new-d (deck-remove-top d)))
            (game-set-current-player j
              (player-add-to-hand (player-set-deck p new-d) carta)))))))
(define accion-robar-2
  (lambda (j . a) (accion-robar-1 (accion-robar-1 j))))
(define accion-curar-30
  (lambda (j . a)
    (let* ((p (game-current-player j)) (act (player-active p)))
      (if (null? act) j
          (game-set-current-player j
            (player-set-active p (ipc-set-damage act (max 0 (- (ipc-damage act) 30)))))))))
(define accion-cambio-libre (lambda (j . a) j))

;; ===================================================================
;; ATAQUES Y HABILIDADES (con "Inflige" para que funcione el daño)
;; ===================================================================
(define a-squir-1 (attack '(water) "Burbuja" "Inflige 10 de daño" accion-noop))
(define a-pika-1  (attack '(lightning) "Onda Eléctrica" "Inflige 10 de daño y paraliza" accion-paralizar))
(define a-pika-2  (attack '(lightning lightning) "Trueno" "Inflige 40 de daño" accion-noop))
(define hab-pika  (ability "Recolectar Energía" "Roba 1 carta del mazo" accion-robar-1))
(define a-magn-1  (attack '(colorless) "Tacleada" "Inflige 10 de daño" accion-noop))
(define a-magnet-1 (attack '(lightning lightning) "Doble Rayo" "Inflige 60 de daño" accion-noop))
(define a-wart-1  (attack '(water water) "Hidrobomba" "Inflige 50 de daño" accion-noop))
(define a-lapras-1 (attack '(water water) "Aqua Splash" "Inflige 80 de daño" accion-noop))
(define a-lapras-2 (attack '(water water water) "Hyper Beam" "Inflige 120 de daño" accion-noop))
(define a-cater-1 (attack '(grass) "Hilo de Seda" "Inflige 10 de daño" accion-noop))
(define a-geo-1   (attack '(fighting) "Lanzamiento" "Inflige 20 de daño" accion-noop))
(define a-mankey-1 (attack '(fighting) "Karate" "Inflige 20 de daño" accion-noop))
(define a-mankey-2 (attack '(fighting colorless) "Mirada Mental" "Inflige 30 de daño y confunde" accion-confundir))
(define hab-mankey (ability "Furia" "Envenena al Pokémon activo del rival" accion-envenenar))
(define a-meta-1  (attack '(grass grass) "Caparazón" "Inflige 30 de daño" accion-noop))
(define a-grav-1  (attack '(fighting fighting) "Roca Pesada" "Inflige 60 de daño" accion-noop))
(define a-tauros-1 (attack '(fighting fighting) "Embestida" "Inflige 90 de daño" accion-noop))
(define a-tauros-2 (attack '(fighting fighting fighting) "Furia Animal" "Inflige 150 de daño" accion-noop))


;; CARTAS 

(define c-squir   (card 'pokemon "Squirtle"  null 50 'water 'lightning null 1 #f null (list a-squir-1)))
(define c-pika    (card 'pokemon "Pikachu"   null 60 'lightning 'fighting null 1 #f hab-pika (list a-pika-1 a-pika-2)))
(define c-magn    (card 'pokemon "Magnemite" null 40 'lightning 'fighting null 1 #f null (list a-magn-1)))
(define c-magnet  (card 'pokemon "Magneton"  "Magnemite" 90 'lightning 'fighting null 2 #f null (list a-magn-1 a-magnet-1)))
(define c-wart    (card 'pokemon "Wartortle" "Squirtle" 80 'water 'lightning null 1 #f null (list a-wart-1)))
(define c-lapras  (card 'pokemon "Lapras EX" null 170 'water 'lightning null 3 #t null (list a-lapras-1 a-lapras-2)))
(define c-cater   (card 'pokemon "Caterpie"  null 50 'grass 'fire null 1 #f null (list a-cater-1)))
(define c-geo     (card 'pokemon "Geodude"   null 60 'fighting 'water null 1 #f null (list a-geo-1)))
(define c-mankey  (card 'pokemon "Mankey"    null 60 'fighting 'psychic null 1 #f hab-mankey (list a-mankey-1 a-mankey-2)))
(define c-meta    (card 'pokemon "Metapod"   "Caterpie" 80 'grass 'fire null 2 #f null (list a-meta-1)))
(define c-grav    (card 'pokemon "Graveler"  "Geodude" 90 'fighting 'water null 2 #f null (list a-grav-1)))
(define c-tauros  (card 'pokemon "Tauros EX" null 170 'fighting 'psychic null 3 #t null (list a-tauros-1 a-tauros-2)))

(define e-water (card 'energy "Agua" 'water))
(define e-light (card 'energy "Eléctrica" 'lightning))
(define e-grass (card 'energy "Planta" 'grass))
(define e-fight (card 'energy "Lucha" 'fighting))

(define t-pocion (card 'trainer "Poción" "objeto" "Cura 30 PS al Pokémon activo" accion-curar-30))
(define t-switch (card 'trainer "Switch" "objeto" "Cambia" accion-cambio-libre))
(define t-prof   (card 'trainer "Profesor" "partidario" "Roba 2" accion-robar-2))
(define t-search (card 'trainer "Búsqueda" "objeto" "Roba 1" accion-robar-1))
(define t-cambio (card 'trainer "Cambio"   "objeto" "Cambia" accion-cambio-libre))
(define t-bertha (card 'trainer "Bertha"   "partidario" "Roba 2" accion-robar-2))


;; MAZOS  semilla 50

(define mazo1 (deck
  c-squir  c-squir  c-squir  c-squir
  c-pika   c-pika   c-pika   c-pika
  c-magn   c-magn   c-magn   c-magn
  c-wart   c-wart   c-wart
  c-magnet c-magnet c-magnet
  c-lapras c-lapras
  t-pocion t-pocion t-pocion
  t-switch t-switch t-switch
  t-prof   t-prof
  e-water e-water e-water e-water e-water e-water e-water e-water
  e-water e-water e-water e-water e-water e-water e-water e-water
  e-light e-light e-light e-light e-light e-light e-light e-light
  e-light e-light e-light e-light e-light e-light e-light e-light))

(define mazo2 (deck
  c-cater  c-cater  c-cater  c-cater
  c-geo    c-geo    c-geo    c-geo
  c-mankey c-mankey c-mankey c-mankey
  c-meta   c-meta   c-meta
  c-grav   c-grav   c-grav
  c-tauros c-tauros
  t-search t-search t-search
  t-cambio t-cambio t-cambio
  t-bertha t-bertha
  e-grass e-grass e-grass e-grass e-grass e-grass e-grass e-grass
  e-grass e-grass e-grass e-grass e-grass e-grass e-grass e-grass
  e-fight e-fight e-fight e-fight e-fight e-fight e-fight e-fight
  e-fight e-fight e-fight e-fight e-fight e-fight e-fight e-fight))




(display "              PRUEBAS DE initGame                             \n")



;; Prueba 1: initGame con OTRA semilla 

(display "\n--- Prueba 1 initGame: semilla 100  ---\n")
(with-handlers ((exn? (lambda (e) (printf " Error inesperado: ~a~n" (exn-message e)))))
  (let ((g-test (initGame mazo1 mazo2 100)))
    (display " initGame con semilla 100 ejecutado SIN errores\n")
    (printf "   Turno inicial: jugador ~a~n" (game-current-turn g-test))
    (printf "   Estado: ~a~n" (game-state g-test))
    (printf "   Cartas en mano J1: ~a~n" (length (player-hand (game-player1 g-test))))
    (printf "   Cartas en mano J2: ~a~n" (length (player-hand (game-player2 g-test))))
    (printf "   Cartas restantes en mazo J1: ~a~n"
            (length (deck-cards (player-deck (game-player1 g-test)))))))

;; -------------------------------------------------------------------
;; Prueba 2: initGame con un MAZO con 60 energías
;
;; -------------------------------------------------------------------
(display "\n--- Prueba 2 initGame: MAZO de solo energias (60 energías) \n")

(with-handlers ((exn? (lambda (e)
                        (printf " Error capturado: ~a~n" (exn-message e)))))
  (let* ((mazo-malo
           (deck
             e-light
             e-light e-light e-light e-light e-light e-light e-light e-light e-light e-light
             e-light e-light e-light e-light e-light e-light e-light e-light e-light e-light
             e-light e-light e-light e-light e-light e-light e-light e-light e-light e-light
             e-light e-light e-light e-light e-light e-light e-light e-light e-light e-light
             e-light e-light e-light e-light e-light e-light e-light e-light e-light e-light
             e-light e-light e-light e-light e-light e-light e-light e-light e-light))
         (g-test (initGame mazo-malo mazo2 50)))
    
    
    (printf "   Estado: ~a~n" (game-state g-test))
    (printf "   Cartas en mano J1 (mazo malo): ~a~n"
            (length (player-hand (game-player1 g-test))))
    (printf "   Cartas restantes en mazo J1: ~a~n"
            (length (deck-cards (player-deck (game-player1 g-test)))))
    (printf "   Pokémon básico (Pikachu) ~a~n"
            (if (member c-pika (player-hand (game-player1 g-test)))
                "está en la mano (mulligan resuelto)"
                "no está en la mano (caso raro)"))))



;;          PRUEBAS DE shuffleDeck                                 



(display "\n           PRUEBAS SHUFFLEDECK     \n")                        


;                
;; Prueba 1: shuffleDeck 

(display "\n--- Prueba 1 shuffleDeck: input INVÁLIDO  ---\n")
(with-handlers ((exn? (lambda (e)
                        (printf " Error  capturado: ~a~n"
                                (exn-message e)))))
  (let ((d-mal (shuffleDeck "esto-no-es-un-mazo" 50)))
    (display " shuffleDeck aceptó input inválido \n")))


;; Prueba 2: misma semilla 

(display "\n--- Prueba 2 shuffleDeck: misma semilla 77  ---\n")
(with-handlers ((exn? (lambda (e) (printf " Error: ~a~n" (exn-message e)))))
  (let* ((shuf-1 (shuffleDeck mazo1 77))
         (shuf-2 (shuffleDeck mazo1 77))
         (cartas-1 (deck-cards shuf-1))
         (cartas-2 (deck-cards shuf-2)))
    (printf "   Total cartas barajado 1: ~a~n" (length cartas-1))
    (printf "   Total cartas barajado 2: ~a~n" (length cartas-2))
    (printf "   1ra carta barajado 1: ~a~n" (card-name (car cartas-1)))
    (printf "   1ra carta barajado 2: ~a~n" (card-name (car cartas-2)))
    (printf "   2da carta barajado 1: ~a~n" (card-name (cadr cartas-1)))
    (printf "   2da carta barajado 2: ~a~n" (card-name (cadr cartas-2)))
    (if (equal? cartas-1 cartas-2)
        (display "  Las cartas no difieren \n")
        (display " Las cartas difieren \n"))))


;; Prueba 3: OTRA semilla 

(display "\n--- Prueba 3 shuffleDeck: semilla 200  ---\n")
(with-handlers ((exn? (lambda (e) (printf " Error: ~a~n" (exn-message e)))))
  (let* ((shuf-50  (shuffleDeck mazo1 50))
         (shuf-200 (shuffleDeck mazo1 200))
         (cartas-50  (deck-cards shuf-50))
         (cartas-200 (deck-cards shuf-200)))
    (printf "   1ra carta semilla  50: ~a~n" (card-name (car cartas-50)))
    (printf "   1ra carta semilla 200: ~a~n" (card-name (car cartas-200)))
    (printf "   2da carta semilla  50: ~a~n" (card-name (cadr cartas-50)))
    (printf "   2da carta semilla 200: ~a~n" (card-name (cadr cartas-200)))
    (if (equal? cartas-50 cartas-200)
        (display " Las cartas son IGUALES \n")
        (display " Las cartas DIFIEREN )\n"))))


;;               SCRIPT PRINCIPAL DE 8 TURNOS                       



(display "              SCRIPT PRINCIPAL (semilla 50)                   \n")


;; INICIO (semilla 50, forzar turno 1 para J1)
(define g0-raw (initGame mazo1 mazo2 50))
(define g0
  (if (and g0-raw (= (game-current-turn g0-raw) 2))
      (game-swap-turn g0-raw)
      g0-raw))

(display "\n===== ESTADO INICIAL (semilla 50) =====\n")
(displayln (printGame g0 1))
(display "\n")


;; TURNO 1 – J1

(display "===== TURNO 1 - J1 =====\n")
(define g1 (drawCardFromDeck g0)); Roba una carta
(define g2 (playToBench g1 c-magn)) ; magnemite a banca
(define g3 (changeActivePokemon g2 c-magn)) ; magnemite a activo
(define g4 (useEnergyCard g3 c-magn e-light)) ; asigna energia lightning a magnemite
(define g5 (usePokemonAttack g4 c-magn "Tacleada" '())) ; ataque falla no hay rival
(displayln (printGame g5 1))
(display "\n")


;; TURNO 2 – J2

(display "===== TURNO 2 - J2 =====\n")
(define g6  (drawCardFromDeck g5)); roba carta
(define g7  (playToBench g6 c-mankey)); mankey a la banca
(define g8  (changeActivePokemon g7 c-mankey)); mankey a activo
(define g9  (useEnergyCard g8 c-mankey e-fight)); adjunta energia fighting
(define g10 (usePokemonAttack g9 c-mankey null '())); mankey ataque nulo
(displayln (printGame g10 2))
(display "\n")


;; TURNO 3 – J1

(display "===== TURNO 3 - J1 =====\n")
(define g11 (drawCardFromDeck g10)); roba carta
(define g12 (playToBench g11 c-pika)); pikachu a la banca
(define g13 (useTrainerCard g12 t-prof '())); usa carta prof , roba 2 cartas
(define g14 (evolvePokemon g13 c-magn c-magnet)); magnemite evoluciona a magneton
(define g15 (useEnergyCard g14 c-magnet e-light)); magnemite adjunta energia lightning
(define g16 (usePokemonAttack g15 c-magnet "Tacleada" '())); ataca a mankey -10
(displayln (printGame g16 1))
(display "\n")


;; TURNO 4 – J2

(display "===== TURNO 4 - J2 =====\n")
(define g17 (drawCardFromDeck g16)); roba carta
(define g18 (playToBench g17 c-cater)); error caterpie no esta en el mazo
(define g19 (evolvePokemon g18 c-cater c-meta)); caterpie no evoluciona no esta en el mazo
(define g20 (usePokemonAbility g19 c-mankey '())); mankey ocupa su habilidad
(define g21 (useEnergyCard g20 c-mankey e-fight)); mankey adjunta fighting
(define g22 (usePokemonAttack g21 c-mankey "Mirada Mental" '())) ;mankey ataca con mirada mental magneton -60 estado confundido
(displayln (printGame g22 2))
(display "\n")


;; TURNO 5 – J1


(display " TURNO 5 - J1  \n")
(define g23 (drawCardFromDeck g22)); saca carta


(define g23b
  (let ((p (game-current-player g23)))
      (game-set-current-player g23 (player-add-to-hand p t-pocion))))


(define g24 (useTrainerCard g23b t-pocion '())); pocion cura en 30 a magneton
(define g25 (changeActivePokemon g24 c-wart))      ; falla (no en juego)
(define g26 (usePokemonAbility g25 c-pika '()))    ; Recolectar (banca)
(define g27 (useEnergyCard g26 c-magnet e-water))  ; energía Agua
(define g28 (usePokemonAttack g27 c-magnet "Tacleada" '())) ; 10 daño a mankey
(displayln (printGame g28 1))
(display "\n")


;; TURNO 6 – J2

(display "===== TURNO 6 - J2 =====\n")
(define g29 (drawCardFromDeck g28)); roba carta
(define g30 (playToBench g29 c-cater)); mueve a caterpie a banca
(define g31 (useEnergyCard g30 c-meta e-grass)); adjunta planta a metapod falla no esta en juego
(define g32 (useEnergyCard g31 c-meta e-grass));adjunta planta a metapod falla no esta en juego
(define g33 (usePokemonAttack g32 c-meta "Caparazón" '())); metapod ataca pero no esta en juego
(displayln (printGame g33 2))
(display "\n")


;; TURNO 7 

(display "===== TURNO 7 - J1 =====\n")
(define g34 (drawCardFromDeck g33)); roba carta
(define g35 (evolvePokemon g34 c-squir c-wart)); evoluciona a squirtle pero no esta activo
(define g36 (useTrainerCard g35 t-switch '())); usa carta entrenador
(define g37 (usePokemonAbility g36 c-pika '())); usa habilidad
(define g38 (useEnergyCard g37 c-magnet e-light)); magneton adjunta lightning
(define g39 (usePokemonAttack g38 c-magnet "Doble Rayo" '())); ataca con doble rayo , esta confundido
(displayln (printGame g39 1))
(display "\n")


;; TURNO 8 

(display "===== TURNO 8 - J2 =====\n")
(define g40 (drawCardFromDeck g39)); roba carta
(define g41 (useEnergyCard g40 c-cater e-grass)); adjunta energia planta
(define g42 (usePokemonAttack g41 c-cater "Hilo de Seda" '())) ; ataca con seda -10 magneton
(displayln (printGame g42 2))
(display "\n")
