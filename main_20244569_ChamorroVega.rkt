
#lang racket


(require "TDA-attack_20244569_ChamorroVega.rkt")
(require "TDA-card_20244569_ChamorroVega.rkt")
(require "TDA-deck_20244569_ChamorroVega.rkt")
(require "TDA-player_20244569_ChamorroVega.rkt")
(require "TDA-game_20244569_ChamorroVega.rkt")

(provide

  (all-from-out "TDA-attack_20244569_ChamorroVega.rkt")
  (all-from-out "TDA-card_20244569_ChamorroVega.rkt")
  (all-from-out "TDA-deck_20244569_ChamorroVega.rkt")
  (all-from-out "TDA-player_20244569_ChamorroVega.rkt")
  (all-from-out "TDA-game_20244569_ChamorroVega.rkt")

 
  playToBench
  changeActivePokemon
  drawCardFromDeck
  useEnergyCard
  usePokemonAttack
  useTrainerCard
  usePokemonAbility
  evolvePokemon)

; Descripción: Convierte el nombre de una carta a string .
; Dom: n (symbol/string)
; Rec: string
;Tipo recursión: No aplica;
(define nombre->str
  (lambda (n)
    (cond
      ((symbol? n) (symbol->string n))
      ((string? n) n)
      (else ""))))

; Descripción: Compara dos nombres de cartas para ver si son exactamente iguales, ignorando si están en mayúsculas o minúsculas.
; Dom: a  X b 
; Rec: boolean
; Tipo recursión: No aplica
(define nombre=?
  (lambda (a b)
    (string-ci=? (nombre->str a) (nombre->str b))))

; Descripción: Revisa rápidamente si un jugador tiene una carta específica en su mano.
; Dom: p (player) X c (card/null)
; Rec: boolean
; Tipo recursión: No aplica
(define (card-in-hand? p c)
  (if (null? c)
      #f
      (buscar-carta? (player-hand p) c)))

; Descripción: Busca paso a paso dentro de la lista de la mano para ver si encuentra la carta pedida.
; Dom: mano (list) X c (card)
; Rec: boolean
; Tipo recursión: Cola
(define (buscar-carta? mano c)
  (if (null? mano)
      #f
      (if (son-la-misma-carta? (car mano) c)
          #t
          (buscar-carta? (cdr mano) c))))

; Descripción: Revisa si dos cartas son idénticas comprobando que tengan el mismo tipo .
; Dom: carta1 (card) X carta2 (card)
; Rec: boolean
; Tipo recursión: No aplica
(define (son-la-misma-carta? carta1 carta2)
  (if (eq? (card-type carta1) (card-type carta2))
      (if (nombre=? (card-name carta1) (card-name carta2))
          #t
          #f)
      #f))

; Descripción: Busca a un pokemon específico que el jugador tenga en juego (ya sea como Activo o en la Banca) .
; Dom: p (player) X c (card/null)
; Rec: list (con la zona y la carta) / boolean (#f si no lo encuentra)
; Tipo recursión: Cola
(define find-pokemon-in-play
  (lambda (p c)
    (cond
      [(null? c) #f]
      [(not (card-pokemon? c)) #f]
      [else
       (let ([name (card-name c)])
         (cond
           
           [(and (not (null? (player-active p)))
                 (nombre=? (card-name (ipc-card (player-active p))) name))
            (list 'active (player-active p))]
           
           [else
            (let loop ([bench (player-bench p)])
              (cond
                [(null? bench) #f]
                [(nombre=? (card-name (ipc-card (car bench))) name)
                 (list 'bench (car bench))]
                [else (loop (cdr bench))]))]))])))

; Descripción: Busca específicamente si un Pokémon se encuentra sentado en la banca del jugador.
; Dom: p (player) X c (card/null)
; Rec: in-play-card / boolean (#f si no lo encuentra)
; Tipo recursión: Cola
(define find-in-bench
  (lambda (p c)
    (cond
      [(null? c) #f]
      [else
       (let ([name (card-name c)])
         (let loop ([bench (player-bench p)])
           (cond
             [(null? bench) #f]
             [(nombre=? (card-name (ipc-card (car bench))) name)
              (car bench)]
             [else (loop (cdr bench))])))])))

; Descripción: Cambia un Pokémon viejo de la banca por uno nuevo .
; Dom: p (player) X old-ipc (in-play-card) X new-ipc (in-play-card)
; Rec: player (list)
; Tipo recursión: Natural 
(define replace-in-bench
  (lambda (p old-ipc new-ipc)
    (let ((old-id (ipc-id old-ipc)))
      (define (reemplazar bench)
        (cond
          ((null? bench) '())
          ((= (ipc-id (car bench)) old-id)
           (cons new-ipc (cdr bench)))
          (else
           (cons (car bench) (reemplazar (cdr bench))))))
      (player-set-bench p (reemplazar (player-bench p))))))

; Descripción: Crea un número identificador único usando la semilla del juego para diferenciar a los pokemon que entran a la mesa.
; Dom: g (game)
; Rec: integer
; Tipo recursión: No aplica
(define gen-unique-id
  (lambda (g)
    (abs (game-seed g))))

; Descripción: Toma un Pokémon que está en juego y lo transforma en su evolución, manteniendo el daño que ya tenía y las energías unidas.
; Dom: ipc (in-play-card) X new-card (card)
; Rec: in-play-card (list)
; Tipo recursión: No aplica
(define evolve-ipc
  (lambda (ipc new-card)
    (list new-card
          (ipc-energies ipc)
          (ipc-damage ipc)
          "normal"
          0
          (ipc-id ipc))))
; Descripción: Revisa la lista de ataques de un pokemon y devuelve el primero que pueda usar con las energías que tiene puestas.
; Dom: ataques (list) X energias (list)
; Rec: attack / boolean (#f si no puede usar ninguno)
; Tipo recursión: Cola

(define find-first-usable-attack
  (lambda (ataques energias)
    (cond
      ((null? ataques) #f)
      ((attack-can-use? (car ataques) energias) (car ataques))
      (else (find-first-usable-attack (cdr ataques) energias)))))
; Descripción:  Busca la palabra "Inflige" en el texto de un ataque y extrae el número de daño que viene justo después.
; Dom: texto (string)
; Rec: integer
; Tipo recursión: No aplica

(define (extract-damage-from-text texto)
  (let ((idx (regexp-match-positions #rx"[Ii]nflige" texto)))
    (cond
      ((not idx) 0)
      (else
       (let ((despues (substring texto (cdr (car idx)))))
         (let ((lista-caracteres (string->list despues)))
           (let ((caracteres-del-numero (buscar-primer-numero lista-caracteres)))
             (if (null? caracteres-del-numero)
                 0
                 (string->number (list->string caracteres-del-numero))))))))))

;Funcion auxiliar
; Descripción: Busca caracter por caracter en un texto hasta encontrar dónde empieza un número.
; Dom: lista (list)
; Rec: list
; Tipo recursión: Cola
(define (buscar-primer-numero lista)
  (if (null? lista)
      '()
      (if (char-numeric? (car lista))
          
          (cons (car lista) (tomar-consecutivos (cdr lista)))
          
          (buscar-primer-numero (cdr lista)))))
;Funcion auxiliar
; Descripción: Una vez que encuentra un número, toma todos los dígitos seguidos (ej: agarra el '5' y luego el '0' para formar '50').
; Dom: lista (list)
; Rec: list
; Tipo recursión: Natural
(define (tomar-consecutivos lista)
  (if (null? lista)
      '()
      (if (char-numeric? (car lista))
          (cons (car lista) (tomar-consecutivos (cdr lista)))
          '())))

; Descripción: Calcula el daño final  de un ataque, aplicando el doble si hay debilidad o restando 30 si hay resistencia.
; Dom: base-dmg (integer) X atk-type (symbol) X def-weakness (symbol/null) X def-resistance (symbol/null)
; Rec: integer
; Tipo recursión: No aplica
(define (compute-damage base-dmg atk-type def-weakness def-resistance)
  (let ((mult (calcular-multiplicador atk-type def-weakness)))
    (let ((red (calcular-reduccion atk-type def-resistance)))
      (max 0 (- (* base-dmg mult) red)))))
;Funcion auxiliar
; Descripción: Revisa si el tipo del ataque es igual a la debilidad del defensor para devolver un multiplicador x2.
; Dom: tipo (symbol) X debilidad (symbol/null)
; Rec: integer
; Tipo recursión: No aplica
(define (calcular-multiplicador tipo debilidad)
  (if (null? debilidad)
      1
      (if (eq? debilidad tipo)
          2
          1)))
;Funcion auxiliar
; Descripción: Revisa si el tipo del ataque es igual a la resistencia del defensor para restar 30 puntos de daño.
; Dom: tipo (symbol) X resistencia (symbol/null)
; Rec: integer
; Tipo recursión: No aplica
(define (calcular-reduccion tipo resistencia)
  (if (null? resistencia)
      0
      (if (eq? resistencia tipo)
          30
          0)))
; Descripción: Quita una cantidad  de energías de un pokemon ).
; Dom: energias (list) X n (integer)
; Rec: list
; Tipo recursión: Cola
(define remove-n-energies
  (lambda (energias n)
    (cond
      [(= n 0) energias]
      [(null? energias) '()]
      [else (remove-n-energies (cdr energias) (- n 1))])))
; Descripción: Saca cartas de los premios del jugador y se las pone en la mano ).
; Dom: p (player) X n (integer)
; Rec: player (list)
; Tipo recursión: Cola

(define take-prizes
  (lambda (p n)
    (cond
      ((<= n 0) p)
      ((null? (player-prizes p)) p)
      (else
       (let ((prize-card (car (player-prizes p))))
         (let ((p1 (player-set-prizes p (cdr (player-prizes p)))))
           (let ((p2 (player-add-to-hand p1 prize-card)))
             (take-prizes p2 (- n 1)))))))))

; Descripción: Si el pokemon activo es derrotado, toma automáticamente al primer pokemmon de la banca y lo pone al frente.
; Dom: p (player)
; Rec: player (list)
; Tipo recursión: No aplica
(define auto-promote-from-bench
  (lambda (p)
    (let ((bench (player-bench p)))
      (cond
        ((null? bench) p)
        (else
         (let* ((nuevo-activo (car bench))
                (p1 (player-set-bench p (cdr bench)))
                (p2 (player-set-active p1 nuevo-activo)))
           p2))))))

; Descripción: Revisa si alguien ganó la partida (porque robó todos sus premios o dejo al oponente sin Pokémon en la mesa).
; Dom: g (game)
; Rec: game (list)
; Tipo recursión: No aplica
(define check-victory
  (lambda (g)
    (let ((p1 (game-player1 g))
          (p2 (game-player2 g)))
      (cond
        ((null? (player-prizes p1)) (game-set-state g "gana-j1"))
        ((null? (player-prizes p2)) (game-set-state g "gana-j2"))
        ((and (null? (player-active p1)) (null? (player-bench p1)))
         (game-set-state g "gana-j2"))
        ((and (null? (player-active p2)) (null? (player-bench p2)))
         (game-set-state g "gana-j1"))
        (else g)))))
; Descripción: Aplica el proceso de mandar a un pokemon derrotado al descarte (junto con sus cartas), robar los premios correspondientes y poner a uno nuevo al frente.
; Dom: g (game) X ko-player-num (integer)
; Rec: game (list)
; Tipo recursión: Cola (en los agregados de cartas)
(define ko-pokemon
  (lambda (g ko-player-num)
    (let* ((ko-player (if (= ko-player-num 1)
                          (game-player1 g)
                          (game-player2 g)))
           (atk-num   (if (= ko-player-num 1) 2 1))
           (atacante  (if (= atk-num 1)
                          (game-player1 g)
                          (game-player2 g)))
           (ko-ipc    (player-active ko-player))
           (ko-card   (ipc-card ko-ipc))
           (is-ex     (card-is-ex? ko-card))
           (n-premios (if is-ex 2 1))
           
           (energias-cartas
            (map (lambda (tipo-e)
                   (card 'energy
                         (string->symbol
                          (string-append (symbol->string tipo-e) "-energy"))))
                 (ipc-energies ko-ipc)))
           
           (ko1       (player-add-to-discard ko-player ko-card))
           
           (ko1b      (let agregar ((es energias-cartas) (acc ko1))
                        (cond
                          ((null? es) acc)
                          (else (agregar (cdr es)
                                         (player-add-to-discard acc (car es)))))))
           (ko2       (player-set-active ko1b null))
           (ko3       (auto-promote-from-bench ko2))
           (atk2      (take-prizes atacante n-premios))
           (g1        (if (= ko-player-num 1)
                          (game-set-player1 g ko3)
                          (game-set-player2 g ko3)))
           (g2        (if (= atk-num 1)
                          (game-set-player1 g1 atk2)
                          (game-set-player2 g1 atk2))))
      (check-victory g2))))
; Descripción: Revisa a ambos jugadores en la mesa para ver si algún pokemon Activo se quedo sin vida y aplica el efecto de derrota.
; Dom: g (game)
; Rec: game (list)
; Tipo recursión: No aplica
(define handle-knockouts
  (lambda (g)
    (let ((g1 (check-and-handle-ko g 1)))
      (cond
        ((game-over? g1) g1)
        (else (check-and-handle-ko g1 2))))))

; Descripción: Verifica específicamente la vida del pokemon Activo de un solo jugador y lo derrota si sus PS llegaron a cero.
; Dom: g (game) X player-num (integer)
; Rec: game (list)
; Tipo recursión: No aplica
(define check-and-handle-ko
  (lambda (g player-num)
    (let ((p (if (= player-num 1) (game-player1 g) (game-player2 g))))
      (let ((activo (player-active p)))
        (cond
          ((null? activo) g)
          ((ipc-is-knocked-out? activo) (ko-pokemon g player-num))
          (else g))))))

; Descripción: Toma un Pokémon Básico de tu mano .
; Dom: g (game) X c (card)
; Rec: game (list)
; Tipo recursión: No aplica
(define playToBench
  (lambda (g c)
    (let ((p (game-current-player g)))
      (cond
        ((game-over? g) g)
        ((not (card-pokemon? c)) g)
        ((not (card-basic-pokemon? c)) g)
        ((player-bench-full? p) g)
        ((not (card-in-hand? p c)) g)
        (else
         (let* ((new-id  (gen-unique-id g))
                (ipc     (make-in-play-card c new-id))
                (p1      (player-remove-from-hand p (card-name c)))
                (p2      (player-add-to-bench p1 ipc))
                (g1      (game-set-current-player g p2))
                (g2      (game-next-seed g1)))
           g2))))))


; Descripción: Retira al pokemon Activo actual, paga su costo de retirada en energías descartadas y pone a otro pokemon de la banca al frente.
; Dom: g (game) X c (card)
; Rec: game (list)
; Tipo recursión: Natural
(define changeActivePokemon
  (lambda (g c)
    (let* ((p       (game-current-player g))
           (activo  (player-active p)))
      (cond
        ((game-over? g) g)
        ((null? c) g)
        ((not (card-pokemon? c)) g)
        
        ((and (not (null? activo))
              (nombre=? (card-name (ipc-card activo)) (card-name c)))
         g)
        (else
         (let ((objetivo (find-in-bench p c)))
           (cond
             ((not objetivo) g)
             
             ((null? activo)
              (let* ((p1 (player-remove-from-bench p (ipc-id objetivo)))
                     (p2 (player-set-active p1 objetivo)))
                (game-set-current-player g p2)))
             
             (else
              
              (let ((retreat (let ((rc (card-retreat-cost (ipc-card activo))))
                               (if (null? rc) 0 rc))))
                (cond
                  ((< (length (ipc-energies activo)) retreat) g)
                  (else
                   (let* ((energias-a-descartar
                           (let pick ((es (ipc-energies activo)) (n retreat))
                             (cond
                               ((or (= n 0) (null? es)) '())
                               (else
                                (cons (card 'energy
                                            (string->symbol
                                             (string-append
                                              (symbol->string (car es))
                                              "-energy")))
                                      (pick (cdr es) (- n 1)))))))
                          (activo-sin-energ (ipc-set-energies
                                             activo
                                             (remove-n-energies (ipc-energies activo)
                                                                retreat)))
                          
                          (activo-nuevo (ipc-set-status activo-sin-energ "normal"))
                          (p1 (player-remove-from-bench p (ipc-id objetivo)))
                          (p2 (player-add-to-bench p1 activo-nuevo))
                          (p3 (player-set-active p2 objetivo))
                          (p4 (let agregar ((es energias-a-descartar) (acc p3))
                                (cond
                                  ((null? es) acc)
                                  (else
                                   (agregar (cdr es)
                                            (player-add-to-discard acc
                                                                   (car es))))))))
                     (game-set-current-player g p4)))))))))))))
; Descripción: Es la acción de robar una carta del mazo al principio del turno. Si no hay cartas pierde.
; Dom: g (game)
; Rec: game (list)
; Tipo recursión: No aplica
(define drawCardFromDeck
  (lambda (g)
    (let* ((p (game-current-player g))
           (d (player-deck p)))
      (cond
        ((game-over? g) g)
        
        ((not (string=? (game-phase g) "inicio")) g)
        ((deck-empty? d)
         
         (game-set-state g
                         (if (= (game-current-turn g) 1) "gana-j2" "gana-j1")))
        (else
         (let* ((carta    (deck-top d))
                (new-deck (deck-remove-top d))
                (p1       (player-set-deck p new-deck))
                (p2       (player-add-to-hand p1 carta))
                (g1       (game-set-current-player g p2))
                (g2       (game-set-phase g1 "preparacion")))
           g2))))))
; Descripción: Toma una carta de energía de tu mano y se la une a uno de los Pokemon en juego. Solo se puede hacer una vez por turno.
; Dom: g (game) X target (card) X energy (card)
; Rec: game (list)
; Tipo recursión: No aplica
(define useEnergyCard
  (lambda (g target energy)
    (let ((p (game-current-player g)))
      (cond
        ((game-over? g) g)
        ((null? energy) g)
        ((not (card-energy? energy)) g)
        
        ((player-energy-used? p) g)
        ((not (card-in-hand? p energy)) g)
        (else
         (let ((info (find-pokemon-in-play p target)))
           (cond
             ((not info) g)
             (else
              (let ((zona (car info)))
                (let ((ipc (cadr info)))
                  (let ((tipo-e (card-energy-type energy)))
                    (let ((ipc-nuevo (ipc-add-energy ipc tipo-e)))
                      (let ((p1 (player-remove-from-hand p (card-name energy))))
                        (let ((p2 (cond
                                    ((eq? zona 'active)
                                     (player-set-active p1 ipc-nuevo))
                                    (else
                                     (replace-in-bench p1 ipc ipc-nuevo)))))
                          (let ((p3 (player-set-energy-used p2 #t)))
                            (game-set-current-player g p3))))))))))))))))


; Descripción: Ejecuta un ataque de tu pokemon activo. Intenta atacar, aplica el daño y efectos, revisa si alguien murió y luego pasa el turno.
; Dom: g (game) X c (card) X attack-name (string) X extra-args (list)
; Rec: game (list)
; Tipo recursión: No aplic
(define usePokemonAttack
  (lambda (g c attack-name extra-args)
    (cond
      ((game-over? g) g)
      
      ((null? attack-name)
       (if (game-over? g) g (game-swap-turn g)))
      (else
       (let ((g-ataque (try-attack g c attack-name extra-args)))
         (let ((g-ko (handle-knockouts g-ataque)))
           (let ((g-final (if (game-over? g-ko) g-ko (game-swap-turn g-ko))))
             g-final)))))))
; Descripción: Revisa matemáticamente si tu Pokémon activo tiene las energías suficientes para hacer el ataque pedido antes de lanzarlo.
; Dom: g (game) X c (card) X attack-name (string) X extraargs (list)
; Rec: game (list)
; Tipo recursión: No aplica
(define try-attack
  (lambda (g c attack-name extra-args)
    (let ((p (game-current-player g)))
      (let ((activo (player-active p)))
        (cond
          ((null? activo) g)
          ((not (card-pokemon? c)) g)
          ((not (nombre=? (card-name (ipc-card activo)) (card-name c))) g)
          ((not (string? attack-name)) g)
          (else
           (let ((ataques (card-attacks (ipc-card activo))))
             (let ((energias (ipc-energies activo)))
               (let ((ataque
                      (let ((a (card-get-attack (ipc-card activo) attack-name)))
                        (cond
                          ((not a) #f)
                          ((attack-can-use? a energias) a)
                          (else #f)))))
                 (cond
                   ((not ataque) g)
                   (else (apply-attack-effects g ataque extra-args))))))))))))
; Descripción: Calcula el daño, aplica debilidades y resistencias, le baja la vida al defensor y activa efectos extra del ataque.
; Dom: g (game) X atk (attack) X extraargs (list)
; Rec: game (list)
; Tipo recursión: No aplica
(define apply-attack-effects
  (lambda (g atk extra-args)
    (let ((p (game-current-player g)))
      (let ((opp (game-opponent g)))
        (let ((atacante (player-active p)))
          (let ((defensor (player-active opp)))
            (cond
              ((null? defensor) g)
              (else
               (let ((atk-type (card-pokemon-type (ipc-card atacante))))
                 (let ((def-card (ipc-card defensor)))
                   (let ((def-weak (card-weakness def-card)))
                     (let ((def-res (card-resistance def-card)))
                       (let ((base-dmg (extract-damage-from-text (attack-text atk))))
                         (let ((final-dmg (compute-damage base-dmg atk-type def-weak def-res)))
                           (let ((def-nuevo (ipc-set-damage defensor (+ (ipc-damage defensor) final-dmg))))
                             (let ((opp-nuevo (player-set-active opp def-nuevo)))
                               (let ((g1 (game-set-opponent g opp-nuevo)))
                                 (let ((fn (attack-function atk)))
                                   (let ((g2 (apply fn (cons g1 extra-args))))
                                     g2)))))))))))))))))))

; Descripción: Juega una carta de Entrenador (Soporte u Objeto) de la mano, aplica su efecto mágico y luego la manda al descarte.
; Dom: g (game) X trainer-card (card) X args (list)
; Rec: game (list)
; Tipo recursión: No aplica
(define useTrainerCard
  (lambda (g trainer-card args)
    (let ((p (game-current-player g)))
      (cond
        ((game-over? g) g)
        ((null? trainer-card) g)
        ((not (card-trainer? trainer-card)) g)
        ((not (card-in-hand? p trainer-card)) g)
        ((and (string=? (card-trainer-type trainer-card) "partidario")
              (player-supporter-used? p))
         g)
        (else
         (let ((fn (card-trainer-function trainer-card)))
           (let ((p1 (player-remove-from-hand p (card-name trainer-card))))
             (let ((p2 (player-add-to-discard p1 trainer-card)))
               (let ((p3 (cond
                           ((string=? (card-trainer-type trainer-card) "partidario")
                            (player-set-supporter-used p2 #t))
                           (else p2))))
                 (let ((g1 (game-set-current-player g p3)))
                   (let ((g2 (apply fn (cons g1 args))))
                     g2)))))))))))
; Descripción: Activa la habilidad especial de un Pokémon . Funciona como un ataque pero sin costo de energía y sin terminar el turno.
; Dom: g (game) X c (card) X args (list)
; Rec: game (list)
; Tipo recursión: No aplica
(define usePokemonAbility
  (lambda (g c args)
    (let ((p (game-current-player g)))
      (cond
        ((game-over? g) g)
        ((null? c) g)
        ((not (card-pokemon? c)) g)
        (else
         (let ((info (find-pokemon-in-play p c)))
           (cond
             ((not info) g)
             (else
              
              (let ((ipc (cadr info)))
                (let ((habilidad (card-ability (ipc-card ipc))))
                  (cond
                    ((null? habilidad) g)
                    ((not (attack? habilidad)) g)
                    (else
                     (let ((fn (attack-function habilidad)))
                       (apply fn (cons g args)))))))))))))))

; Descripción: Toma un Pokémon en juego (Activo o Banca) que lleve al menos 1 turno en la mesa y lo evoluciona usando una carta de tu mano.
; Dom: g (game) X base-card (card) X evolution-card (card)
; Rec: game (list)
; Tipo recursión: No aplica
(define evolvePokemon
  (lambda (g base-card evolution-card)
    (let ((p (game-current-player g)))
      (cond
        ((game-over? g) g)
        ((null? base-card) g)
        ((null? evolution-card) g)
        ((not (card-pokemon? base-card)) g)
        ((not (card-pokemon? evolution-card)) g)
        ((not (card-in-hand? p evolution-card)) g)
        ((null? (card-evolves-from evolution-card)) g)
        ((not (nombre=? (card-evolves-from evolution-card)
                        (card-name base-card)))
         g)
        (else
         (let ((info (find-pokemon-in-play p base-card)))
           (cond
             ((not info) g)
             (else
              (let ((zona (car info))
                    (ipc (cadr info)))
                (cond
                  ((< (ipc-turns-in-play ipc) 1) g)
                  (else
                   (let ((ipc-evol (evolve-ipc ipc evolution-card)))
                     (let ((p1 (player-remove-from-hand p (card-name evolution-card))))
                       (let ((p2 (player-add-to-discard p1 (ipc-card ipc))))
                         (let ((p3 (cond
                                     ((eq? zona 'active)
                                      (player-set-active p2 ipc-evol))
                                     (else
                                      (replace-in-bench p2 ipc ipc-evol)))))
                           (game-set-current-player g p3))))))))))))))))
; Descripción:  muestra un texto en la consola de Racket.
; Dom: s (string)
; Rec: void
; Tipo recursión: No aplica
(define displayIn
  (lambda (s)
    (display s)))
