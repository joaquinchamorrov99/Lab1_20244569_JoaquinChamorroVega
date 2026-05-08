#lang racket


(require "TDA_card.rkt")
(require "TDA_deck.rkt")
(require "TDA_player.rkt")

(provide
  make-game
  game?
  game-player1
  game-player2
  game-current-turn
  game-seed
  game-state
  game-phase
  game-current-player
  game-opponent
  game-set-player1
  game-set-player2
  game-set-turn
  game-set-seed
  game-set-state
  game-set-phase
  game-set-current-player
  game-set-opponent
  game-over?
  game-winner
  game-swap-turn
  game-next-seed
  initGame
  printGame)

;;TDA GAME

;; REPRESENTACIÓN:
;; El juego se representa como una lista de 6 elementos:
;; (list jugador1 jugador2 turno-actual semilla estado-juego fase-turno)
;;
;;   jugador1    : player -> datos del jugador 1
;;   jugador2    : player -> datos del jugador 2
;;   turno-actual: int    -> 1 o 2, indica quién juega ahora
;;   semilla     : int    -> semilla pseudoaleatoria actual
;;   estado-juego: String -> "en-curso", "gana-j1", "gana-j2"
;;   fase-turno  : String -> "inicio", "preparacion"



(define VALID-GAME-STATES '("en-curso" "gana-j1" "gana-j2"))
(define VALID-GAME-PHASES '("inicio" "preparacion"))

; Descripción: Verifica si un string corresponde a un estado de juego válido.
; Dom: s (string/any)
; Rec: boolean
; Tipo recursión: No aplica
(define (valid-game-state? s)
  (if (string? s)
      (esta-en-la-lista? s VALID-GAME-STATES)
      #f))
; Descripción: Busca recursivamente si un estado específico se encuentra dentro de una lista de estados.
; Dom: estado (string) X lista (list)
; Rec: boolean
; Tipo recursión: Cola
(define (esta-en-la-lista? estado lista)
  (if (null? lista)
      #f
      (if (string=? estado (car lista))
          #t
          (esta-en-la-lista? estado (cdr lista)))))

; Descripción: Verifica si un string corresponde a una fase de juego válida .
; Dom: s (string/any)
; Rec: boolean
; Tipo recursión: No aplica
(define (valid-game-phase? s)
  (if (string? s)
      (buscar-fase? s VALID-GAME-PHASES)
      #f))
; Descripción: Busca recursivamente si una fase específica se encuentra dentro de una lista de fases.
; Dom: fase (string) X lista (list)
; Rec: boolean
; Tipo recursión: Cola
(define (buscar-fase? fase lista)
  (if (null? lista)
      #f
      (if (string=? fase (car lista))
          #t
          (buscar-fase? fase (cdr lista)))))

;; GENERADOR PSEUDOALEATORIO

; Descripción: Generador de números pseudoaleatorios .
; Dom: Xn (integer)
; Rec: integer
; Tipo recursión: No aplica
(define randomPuro
  (lambda (Xn)
    (modulo (+ (* Xn 1103515245) 12345) 2147483648)))

;; CONSTRUCTOR

; Descripción: Constructor principal que inicializa el estado del juego con 2 jugadores, un turno inicial, una semilla y un estado/fase.
; Dom: j1 (player) X j2 (player) X turno (integer) X semilla (integer) X estado (string) X fase (string)
; Rec: game (list)
; Tipo recursión: No aplica
(define make-game
  (lambda (j1 j2 turno semilla estado fase)
    (cond
      ((not (valid-game-state? estado))
       (error "make-game: estado inválido" estado))
      ((not (valid-game-phase? fase))
       (error "make-game: fase inválida" fase))
      (else
       (list j1 j2 turno semilla estado fase)))))

;; FUNCIÓN DE PERTENENCIA

; Descripción: Función de pertenencia que verifica si una estructura cumple con todos los requisitos para ser el TDA game.
; Dom: g (any)
; Rec: boolean
; Tipo recursión: No aplica
(define game?
  (lambda (g)
    (and (list? g)
         (= (length g) 6)
         (player? (list-ref g 0))
         (player? (list-ref g 1))
         (member (list-ref g 2) '(1 2))
         (integer? (list-ref g 3))
         (valid-game-state? (list-ref g 4))
         (valid-game-phase? (list-ref g 5)))))

;; SELECTORES

; Descripción: Selector que obtiene al jugador 1.
; Dom: g (game)
; Rec: player (list)
; Tipo recursión: No aplica
(define game-player1
  (lambda (g)
    (list-ref g 0)))

; Descripción: Selector que obtiene al jugador 2.
; Dom: g (game)
; Rec: player (list)
; Tipo recursión: No aplica
(define game-player2
  (lambda (g)
    (list-ref g 1)))

; Descripción: Selector que indica de quién es el turno actual (1 o 2).
; Dom: g (game)
; Rec: integer
; Tipo recursión: No aplica
(define game-current-turn
  (lambda (g)
    (list-ref g 2)))

; Descripción: Selector que obtiene la semilla actual de aleatoriedad del juego.
; Dom: g (game)
; Rec: integer
; Tipo recursión: No aplica
(define game-seed
  (lambda (g)
    (list-ref g 3)))

; Descripción: Selector que obtiene el estado general del juego (ej. "en-curso").
; Dom: g (game)
; Rec: string
; Tipo recursión: No aplica
(define game-state
  (lambda (g)
    (list-ref g 4)))

; Descripción: Selector que obtiene la fase del turno actual.
; Dom: g (game)
; Rec: string
; Tipo recursión: No aplica
(define game-phase
  (lambda (g)
    (list-ref g 5)))

; Descripción: Selector  que retorna la estructura del jugador que tiene el turno actual.
; Dom: g (game)
; Rec: player (list)
; Tipo recursión: No aplica
(define game-current-player
  (lambda (g)
    (if (= (game-current-turn g) 1)
        (game-player1 g)
        (game-player2 g))))
; Descripción: Selector  que retorna la estructura del jugador que NO tiene el turno actual (el rival).
; Dom: g (game)
; Rec: player (list)
; Tipo recursión: No aplica

(define game-opponent
  (lambda (g)
    (if (= (game-current-turn g) 1)
        (game-player2 g)
        (game-player1 g))))

;; MODIFICADORES

; Descripción: Modificador base que reconstruye la lista del juego actualizando el valor en un índice específico.
; Dom: g (game) X idx (integer) X valor (any)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-set-field
  (lambda (g idx valor)
    (define (rebuild lst i)
      (cond
        ((null? lst) '())
        ((= i idx) (cons valor (cdr lst)))
        (else (cons (car lst) (rebuild (cdr lst) (+ i 1))))))
    (rebuild g 0)))

  (lambda (g j1)
    (game-set-field g 0 j1))
; Descripción: Modificador que actualiza el estado del jugador 1 en el juego.
; Dom: g (game) X j1 (player)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-set-player1
  (lambda (g j1)
    (game-set-field g 0 j1)))
; Descripción: Modificador que actualiza el estado del jugador 2 en el juego.
; Dom: g (game) X j2 (player)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-set-player2
  (lambda (g j2)
    (game-set-field g 1 j2)))
; Descripción: Modificador que cambia de quién es el turno (1 o 2).
; Dom: g (game) X turno (integer)
; Rec: game (list)
; Tipo recursión: No aplica

(define game-set-turn
  (lambda (g turno)
    (game-set-field g 2 turno)))

; Descripción: Modificador que actualiza la semilla del juego para la próxima generación aleatoria.
; Dom: g (game) X semilla (integer)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-set-seed
  (lambda (g semilla)
    (game-set-field g 3 semilla)))

; Descripción: Modificador que actualiza el estado general de la partida .
; Dom: g (game) X estado (string)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-set-state
  (lambda (g estado)
    (if (not (valid-game-state? estado))
        (error "game-set-state: estado inválido" estado)
        (game-set-field g 4 estado))))

; Descripción: Modificador que actualiza la fase en la que se encuentra el turno.
; Dom: g (game) X fase (string)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-set-phase
  (lambda (g fase)
    (if (not (valid-game-phase? fase))
        (error "game-set-phase: fase inválida" fase)
        (game-set-field g 5 fase))))

; Descripción: Modificador que sobreescribe la estructura completa del jugador cuyo turno está en curso.
; Dom: g (game) X p (player)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-set-current-player
  (lambda (g p)
    (if (= (game-current-turn g) 1)
        (game-set-player1 g p)
        (game-set-player2 g p))))

; Descripción: Modificador que sobreescribe la estructura del jugador rival.
; Dom: g (game) X p (player)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-set-opponent
  (lambda (g p)
    (if (= (game-current-turn g) 1)
        (game-set-player2 g p)
        (game-set-player1 g p))))

;; OTRAS FUNCIONES

; Descripción: Verifica si la partida ha finalizado (.
; Dom: g (game)
; Rec: boolean
; Tipo recursión: No aplica
(define game-over?
  (lambda (g)
    (not (string=? (game-state g) "en-curso"))))

; Descripción: Verifica el estado para determinar qué jugador ganó, devolviendo su número (1 o 2), o #f si no ha terminado.
; Dom: g (game)
; Rec: integer/boolean
; Tipo recursión: No aplica
(define game-winner
  (lambda (g)
    (cond
      [(string=? (game-state g) "gana-j1") 1]
      [(string=? (game-state g) "gana-j2") 2]
      [else #f])))

; Descripción: Cambia el turno al jugador contrario..
; Dom: g (game)
; Rec: game (list)
; Tipo recursión: No aplica (aplica modificadores en cadena)
(define (game-swap-turn g)
  (let ((nuevo-turno (if (= (game-current-turn g) 1) 2 1)))
    (let ((g-paso-1 (game-set-turn g nuevo-turno)))
      (let ((g-paso-2 (game-set-phase g-paso-1 "inicio")))
        (let ((nuevo-jugador (game-current-player g-paso-2)))
          (let ((jugador-limpio (player-reset-turn-flags nuevo-jugador)))
            (let ((jugador-listo (player-increment-bench-turns jugador-limpio)))
              (game-set-current-player g-paso-2 jugador-listo))))))))
; Descripción: Actualiza la semilla del juego generando el siguiente número aleatorio de la secuencia.
; Dom: g (game)
; Rec: game (list)
; Tipo recursión: No aplica
(define game-next-seed
  (lambda (g)
    (game-set-seed g (randomPuro (game-seed g)))))

; Descripción: Extrae  cartas del mazo de un jugador y las agrega a su mano, devolviendo al jugador modificado.
; Dom: p (player) X n (integer) X acc (integer)
; Rec: player (list)
; Tipo recursión: Cola
(define (draw-n-cards p n acc)
  (if (= acc n)
      p
      (if (deck-empty? (player-deck p))
          (error "draw-n-cards: mazo vacío ")
          (let ((carta (deck-top (player-deck p))))
            (let ((nuevo-mazo (deck-remove-top (player-deck p))))
              (let ((p2 (player-set-deck p nuevo-mazo)))
                (let ((p3 (player-add-to-hand p2 carta)))
                  (draw-n-cards p3 n (+ acc 1)))))))))
; Descripción: Extrae  cartas del mazo de un jugador y las coloca como sus cartas de premio.
; Dom: p (player) X n (integer) X acc (integer)
; Rec: player (list)
; Tipo recursión: Cola
(define (draw-prizes p n acc)
  (if (= acc n)
      p
      (if (deck-empty? (player-deck p))
          (error "draw-prizes: mazo vacío antes de completar los premios")
          (let ((carta (deck-top (player-deck p))))
            (let ((nuevo-mazo (deck-remove-top (player-deck p))))
              (let ((p2 (player-set-deck p nuevo-mazo)))
                (let ((p3 (player-set-prizes p2 (append (player-prizes p2) (list carta)))))
                  (draw-prizes p3 n (+ acc 1)))))))))

; Descripción: Baraja el mazo, extrae 7 cartas a la mano y 6 de premio. Si no obtiene un Pokémon básico en la mano, reinicia el proceso de barajar.
; Dom: p (player) X semilla (integer)
; Rec: list (contiene player actualizado y nueva semilla)
; Tipo recursión: Cola

(define (setup-player-hand p semilla)
  (let ((nueva-semilla (randomPuro semilla)))
    (let ((mazo-mezclado (shuffleDeck (player-deck p) nueva-semilla)))
      (let ((p-con-mazo (player-set-deck p mazo-mezclado)))
        (let ((p-limpio (player-set-hand p-con-mazo '())))
          (let ((p-con-mano (draw-n-cards p-limpio 7 0)))
            (if (player-has-basics? p-con-mano)
                (let ((p-final (draw-prizes p-con-mano 6 0)))
                  (list p-final nueva-semilla))
                
                (setup-player-hand p nueva-semilla))))))))
; Descripción: maneja la creación completa de un juego nuevo: recibe dos mazos, crea a los jugadores, realiza el setup de robo inicial y determina aleatoriamente quién empieza.
; Dom: d1 (deck) X d2 (deck) X semilla (integer)
; Rec: game (list)
; Tipo recursión: No aplica
(define (initGame d1 d2 semilla)
  (if (deck? d1)
      (if (deck? d2)
          (let ((p1-base (make-player 1 '() '() null '() '() d1)))
            (let ((resultado-j1 (setup-player-hand p1-base semilla)))
              (let ((p1-listo (car resultado-j1)))
                (let ((semilla-j1 (car (cdr resultado-j1))))
                  (let ((p2-base (make-player 2 '() '() null '() '() d2)))
                    (let ((resultado-j2 (setup-player-hand p2-base semilla-j1)))
                      (let ((p2-listo (car resultado-j2)))
                        (let ((semilla-j2 (car (cdr resultado-j2))))
                          (let ((semilla-moneda (randomPuro semilla-j2)))
                            (let ((primer-turno (if (= (modulo semilla-moneda 2) 0) 1 2)))
                              (make-game p1-listo 
                                         p2-listo 
                                         primer-turno 
                                         semilla-moneda 
                                         "en-curso" 
                                         "inicio")))))))))))
              (error "initGame: se requieren dos mazos válidos"))
          (error "initGame: se requieren dos mazos validos")))
; Descripción: Genera un bloque de texto que visualiza la cabecera de información general de la partida .
; Dom: g (game)
; Rec: string
; Tipo recursión: No aplica
(define game-header->string
  (lambda (g)
    (string-append
     "══════════════════════════════════════════════\n"
     "         Estado del juego\n"
     "         Turno del jugador : " (number->string (game-current-turn g)) "\n"
     "         Fase del turno    : " (game-phase g) "\n"
     (if (game-over? g)
         (string-append
          "         JUEGO TERMINADO Ganó el jugador "
          (number->string (game-winner g)) "\n")
         "")
     "══════════════════════════════════════════════\n")))

; Descripción: Genera un string que envuelve la representación en texto de un jugador
; Dom: p (player) X mostrar-mano (boolean) X etiqueta (string)
; Rec: string
; Tipo recursión: No aplica
(define player-block->string
  (lambda (p mostrar-mano etiqueta)
    (string-append
     etiqueta "\n"
     (player->string p mostrar-mano)
     "└──────────────────────────────────────────────┘\n")))

; Descripción: Formatea y retorna la representación visual completa de la mesa de juego para un jugador .
; Dom: g (game) X numero-jugador (integer)
; Rec: string
; Tipo recursión: No aplica
(define (printGame g numero-jugador)
  (let ((j1 (game-player1 g)))
    (let ((j2 (game-player2 g)))
      (string-append
       (game-header->string g)
       "\n"
       
       ;; Bloque del Jugador 1
       (if (= numero-jugador 1)
           (player-block->string j1 #t "───────── JUGADOR 1 (Tu) ────────────────────────────────_")
           (player-block->string j1 #f "───────── JUGADOR 1 (Rival) ─────────────────────────────_"))
       "\n"
       
       ;; Bloque del Jugador 2
       (if (= numero-jugador 2)
           (player-block->string j2 #t "───────── JUGADOR 2 (Tu) ─────────────────────────────────_")
           (player-block->string j2 #f "───────── JUGADOR 2 (Rival) ──────────────────────────────_"))
       "\n"
       "══════════════════════════════════════════════\n"))))
