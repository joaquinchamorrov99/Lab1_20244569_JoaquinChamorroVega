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



(define VALID-GAME-STATES '("en-curso" "gana-j1" "gana-j2"))
(define VALID-GAME-PHASES '("inicio" "preparacion"))


(define (valid-game-state? s)
  (if (string? s)
      (esta-en-la-lista? s VALID-GAME-STATES)
      #f))

(define (esta-en-la-lista? estado lista)
  (if (null? lista)
      #f
      (if (string=? estado (car lista))
          #t
          (esta-en-la-lista? estado (cdr lista)))))


(define (valid-game-phase? s)
  (if (string? s)
      (buscar-fase? s VALID-GAME-PHASES)
      #f))

(define (buscar-fase? fase lista)
  (if (null? lista)
      #f
      (if (string=? fase (car lista))
          #t
          (buscar-fase? fase (cdr lista)))))

(define randomPuro
  (lambda (Xn)
    (modulo (+ (* Xn 1103515245) 12345) 2147483648)))


(define make-game
  (lambda (j1 j2 turno semilla estado fase)
    (cond
      ((not (valid-game-state? estado))
       (error "make-game: estado inválido" estado))
      ((not (valid-game-phase? fase))
       (error "make-game: fase inválida" fase))
      (else
       (list j1 j2 turno semilla estado fase)))))


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


(define game-player1
  (lambda (g)
    (list-ref g 0)))


(define game-player2
  (lambda (g)
    (list-ref g 1)))


(define game-current-turn
  (lambda (g)
    (list-ref g 2)))


(define game-seed
  (lambda (g)
    (list-ref g 3)))


(define game-state
  (lambda (g)
    (list-ref g 4)))


(define game-phase
  (lambda (g)
    (list-ref g 5)))


(define game-current-player
  (lambda (g)
    (if (= (game-current-turn g) 1)
        (game-player1 g)
        (game-player2 g))))


(define game-opponent
  (lambda (g)
    (if (= (game-current-turn g) 1)
        (game-player2 g)
        (game-player1 g))))


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

(define game-set-player1
  (lambda (g j1)
    (game-set-field g 0 j1)))

(define game-set-player2
  (lambda (g j2)
    (game-set-field g 1 j2)))


(define game-set-turn
  (lambda (g turno)
    (game-set-field g 2 turno)))


(define game-set-seed
  (lambda (g semilla)
    (game-set-field g 3 semilla)))


(define game-set-state
  (lambda (g estado)
    (if (not (valid-game-state? estado))
        (error "game-set-state: estado inválido" estado)
        (game-set-field g 4 estado))))


(define game-set-phase
  (lambda (g fase)
    (if (not (valid-game-phase? fase))
        (error "game-set-phase: fase inválida" fase)
        (game-set-field g 5 fase))))


(define game-set-current-player
  (lambda (g p)
    (if (= (game-current-turn g) 1)
        (game-set-player1 g p)
        (game-set-player2 g p))))


(define game-set-opponent
  (lambda (g p)
    (if (= (game-current-turn g) 1)
        (game-set-player2 g p)
        (game-set-player1 g p))))


(define game-over?
  (lambda (g)
    (not (string=? (game-state g) "en-curso"))))


(define game-winner
  (lambda (g)
    (cond
      [(string=? (game-state g) "gana-j1") 1]
      [(string=? (game-state g) "gana-j2") 2]
      [else #f])))


(define (game-swap-turn g)
  (let ((nuevo-turno (if (= (game-current-turn g) 1) 2 1)))
    (let ((g-paso-1 (game-set-turn g nuevo-turno)))
      (let ((g-paso-2 (game-set-phase g-paso-1 "inicio")))
        (let ((nuevo-jugador (game-current-player g-paso-2)))
          (let ((jugador-limpio (player-reset-turn-flags nuevo-jugador)))
            (let ((jugador-listo (player-increment-bench-turns jugador-limpio)))
              (game-set-current-player g-paso-2 jugador-listo))))))))

(define game-next-seed
  (lambda (g)
    (game-set-seed g (randomPuro (game-seed g)))))


(define (draw-n-cards p n acc)
  (if (= acc n)
      p
      (if (deck-empty? (player-deck p))
          (error "draw-n-cards: mazo vacío antes de completar el robo")
          (let ((carta (deck-top (player-deck p))))
            (let ((nuevo-mazo (deck-remove-top (player-deck p))))
              (let ((p2 (player-set-deck p nuevo-mazo)))
                (let ((p3 (player-add-to-hand p2 carta)))
                  (draw-n-cards p3 n (+ acc 1)))))))))

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
              (error "initGame: se requieren dos decks válidos"))
          (error "initGame: se requieren dos decks válidos")))

(define game-header->string
  (lambda (g)
    (string-append
     "══════════════════════════════════════════════\n"
     "         ESTADO DEL JUEGO\n"
     "         Turno del jugador : " (number->string (game-current-turn g)) "\n"
     "         Fase del turno    : " (game-phase g) "\n"
     (if (game-over? g)
         (string-append
          "         ¡JUEGO TERMINADO! Ganó el jugador "
          (number->string (game-winner g)) "\n")
         "")
     "══════════════════════════════════════════════\n")))


(define player-block->string
  (lambda (p mostrar-mano etiqueta)
    (string-append
     etiqueta "\n"
     (player->string p mostrar-mano)
     "└──────────────────────────────────────────────┘\n")))


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
