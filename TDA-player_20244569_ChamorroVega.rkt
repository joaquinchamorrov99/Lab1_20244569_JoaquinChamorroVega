#lang racket



(require "TDA-card_20244569_ChamorroVega.rkt")
(require "TDA-deck_20244569_ChamorroVega.rkt")

(provide
  
  make-in-play-card
  in-play-card?
  ipc-card
  ipc-energies
  ipc-damage
  ipc-status
  ipc-turns-in-play
  ipc-id
  ipc-set-energies
  ipc-set-damage
  ipc-set-status
  ipc-add-turn
  ipc-add-energy
  ipc-remove-energy
  ipc-current-hp
  ipc-is-knocked-out?
  ipc->string

  
  make-player
  player?
  player-id
  player-hand
  player-bench
  player-active
  player-prizes
  player-discard
  player-deck
  player-energy-used?
  player-supporter-used?

  player-set-hand
  player-set-bench
  player-set-active
  player-set-prizes
  player-set-discard
  player-set-deck
  player-set-energy-used
  player-set-supporter-used

  player-add-to-hand
  player-remove-from-hand
  player-add-to-bench
  player-remove-from-bench
  player-add-to-discard
  player-bench-full?
  player-has-basics?
  player-reset-turn-flags
  player-increment-bench-turns
  player->string)


; TDA player
; Se representa internamente como una lista exacta de 9 elementos:
; 1. ID             (integer)           : Identificador del jugador ().
; 2. Mano           (list)              : Lista de cartas en la mano.
; 3. Banca          (list)              : Lista de in-play-cards en la banca.
; 4. Activo         (in-play-card/null) : El pokemon activo actual en combate.
; 5. Premios        (list)              : Lista de cartas de premio apartadas.
; 6. Descarte       (list)              : Lista de cartas en la pila de descarte.
; 7. Mazo           (deck)              : Estructura del TDA deck.
; 8. Energía usada  (boolean)           : #t si ya unio energía en su turno.
; 9. Soporte usado  (boolean)           : #t si ya uso carta de partidario.


;TDA in-play-card
;; ESTADOS VÁLIDOS DE UN POKÉMON

(define VALID-STATUS '("normal" "dormido" "paralizado" "confundido" "envenenado"))

; Descripción: Verifica si el valor entregado es un string que corresponde a un estado válido.
; Dom: s (any)
; Rec: booleano
; Tipo recursión: No aplica
(define (valid-status? s)
  (if (string? s)
      (buscar-estado? (string-downcase s) VALID-STATUS)
      #f))
; Descripción: Busca recursivamente si un estado específico se encuentra dentro de una lista de estados.
; Dom: estado (string) X lista (list)
; Rec: booleano
; Tipo recursión: Cola
(define (buscar-estado? estado lista)
  (if (null? lista)
      #f
      (if (string=? estado (car lista))
          #t
          (buscar-estado? estado (cdr lista)))))
;; CONSTRUCTOR in-play-card
; Descripción: Constructor que crea una carta en juego (in-play-card) a partir de una carta pokemon y un ID.
; Dom: c (card) X id-unico (integer)
; Rec: in-play-card (list)
; Tipo recursión: No aplica



(define make-in-play-card
  (lambda (c id-unico)
    (if (not (card-pokemon? c))
        (error "make-in-play-card: solo se pueden poner en juego cartas Pokémon")
        (list c '() 0 "normal" 0 id-unico))))

;; FUNCIÓN DE PERTENENCIA in-play-card
; Descripción: Función de pertenencia que verifica si la estructura cumple con el TDA in-play-card.
; Dom: ipc (any)
; Rec: boolean
; Tipo recursión: No aplica

(define in-play-card?
  (lambda (ipc)
    (and (list? ipc)
         (= (length ipc) 6)
         (card-pokemon? (list-ref ipc 0))
         (list? (list-ref ipc 1))
         (and (integer? (list-ref ipc 2)) (>= (list-ref ipc 2) 0))
         (valid-status? (list-ref ipc 3))
         (and (integer? (list-ref ipc 4)) (>= (list-ref ipc 4) 0))
         (integer? (list-ref ipc 5)))))

;; SELECTORES in-play-card

; Descripción: Selector que obtiene la carta Pokémon original de un in-play-card.
; Dom: ipc (in-play-card)
; Rec: card
; Tipo recursión: No aplica

(define ipc-card
  (lambda (ipc)
    (list-ref ipc 0)))

; Descripción: Selector que obtiene la lista de energías unidas a un in-play-card.
; Dom: ipc (in-play-card)
; Rec: list
; Tipo recursión: No aplica

(define ipc-energies
  (lambda (ipc)
    (list-ref ipc 1)))

; Descripción: Selector que obtiene el daño acumulado de un in-play-card.
; Dom: ipc (in-play-card)
; Rec: integer
; Tipo recursión: No aplica

(define ipc-damage
  (lambda (ipc)
    (list-ref ipc 2)))

; Descripción: Selector que obtiene el estado actual  de un in-play-card.
; Dom: ipc (in-play-card)
; Rec: string
; Tipo recursión: No aplica

(define ipc-status
  (lambda (ipc)
    (list-ref ipc 3)))

; Descripción: Selector que obtiene los turnos que la carta lleva en juego.
; Dom: ipc (in-play-card)
; Rec: integer
; Tipo recursión: No aplica

(define ipc-turns-in-play
  (lambda (ipc)
    (list-ref ipc 4)))

; Descripción: Selector que obtiene el identificador único de un in-play-card.
; Dom: ipc (in-play-card)
; Rec: integer
; Tipo recursión: No aplica
(define ipc-id
  (lambda (ipc)
    (list-ref ipc 5)))

;; MODIFICADORES in-play-card

; Descripción: Modificador base que reconstruye la lista del in-play-card actualizando un índice específico.
; Dom: ipc (in-play-card) X idx (integer) X valor 
; Rec: in-play-card (lista)
; Tipo recursión: No aplica

(define ipc-set-field
  (lambda (ipc idx valor)
    (define (rebuild lst i)
      (cond
        ((null? lst) '())
        ((= i idx) (cons valor (cdr lst)))
        (else (cons (car lst) (rebuild (cdr lst) (+ i 1))))))
    (rebuild ipc 0)))

; Descripción: Modificador que actualiza la lista de energías de un in-play-card.
; Dom: ipc (in-play-card) X energias (lista)
; Rec: in-play-card (lista)
; Tipo recursión: No aplica

(define ipc-set-energies
  (lambda (ipc energias)
    (ipc-set-field ipc 1 energias)))

; Descripción: Modificador que actualiza el daño de un in-play-card.
; Dom: ipc (in-play-card) X daño (integer)
; Rec: in-play-card (list)
; Tipo recursión: No aplica


(define ipc-set-damage
  (lambda (ipc daño)
    (ipc-set-field ipc 2 daño)))

; Descripción: Modificador que actualiza el estado de un in-play-card, validando que sea correcto.
; Dom: ipc (in-play-card) X estado (string)
; Rec: in-play-card (list)
; Tipo recursión: No aplica

(define ipc-set-status
  (lambda (ipc estado)
    (if (not (valid-status? estado))
        (error "ipc-set-status: estado inválido" estado)
        (ipc-set-field ipc 3 (string-downcase estado)))))

; Descripción: Modificador que incrementa en 1 los turnos en juego de la carta.
; Dom: ipc (in-play-card)
; Rec: in-play-card (lista)
; Tipo recursión: No aplica

(define ipc-add-turn
  (lambda (ipc)
    (ipc-set-field ipc 4 (+ (ipc-turns-in-play ipc) 1))))


; Descripción: Modificador que añade una nueva energía a la lista de energías del in-play-card.
; Dom: ipc (in-play-card) X tipo-energia (symbol)
; Rec: in-play-card (list)
; Tipo recursión: No aplica

(define ipc-add-energy
  (lambda (ipc tipo-energia)
    (ipc-set-energies ipc
                      (cons tipo-energia (ipc-energies ipc)))))

; Descripción: Modificador que elimina la primera ocurrencia de un tipo de energía del in-play-card.
; Dom: ipc (in-play-card) X tipo-energia (symbol)
; Rec: in-play-card (list)
; Tipo recursión: No aplica
(define ipc-remove-energy
  (lambda (ipc tipo-energia)
    (define (remove-first lst tipo encontrado)
      (cond
        ((null? lst) '())
        ((and (not encontrado)
              (eq? (car lst) tipo))
         (remove-first (cdr lst) tipo #t))
        (else
         (cons (car lst) (remove-first (cdr lst) tipo encontrado)))))
    (ipc-set-energies ipc
                      (remove-first (ipc-energies ipc) tipo-energia #f))))

; Descripción: Selector derivado que calcula los puntos de salud restantes del Pokémon.
; Dom: ipc (in-play-card)
; Rec: integer
; Tipo recursión: No aplica

(define ipc-current-hp
  (lambda (ipc)
    (max 0 (- (card-hp (ipc-card ipc)) (ipc-damage ipc)))))

; Descripción: Función de pertenenciaque verifica si el pokemon se ha quedado sin vida.
; Dom: ipc (in-play-card)
; Rec: booleano
; Tipo recursión: No aplica

(define ipc-is-knocked-out?
  (lambda (ipc)
    (= (ipc-current-hp ipc) 0)))

;; OTRAS FUNCIONES in-play-card
; Descripción: Selector que obtiene los tipos de energía unidos (alias de ipc-energies).
; Dom: ipc (in-play-card)
; Rec: list

(define ipc-energy-types
  (lambda (ipc)
    (ipc-energies ipc)))

; Descripción: Convierte una lista de símbolos de energía en una cadena de texto separada por comas.
; Dom: energias (list) X acc (string)
; Rec: string
; Tipo recursión: Cola
(define energies->string
  (lambda (energias acc)
    (cond
      ((null? energias) (if (string=? acc "") "ninguna" acc))
      ((string=? acc "")
       (energies->string (cdr energias) (symbol->string (car energias))))
      (else
       (energies->string (cdr energias)
                         (string-append acc ", "
                                        (symbol->string (car energias))))))))

; Descripción: Convierte la información completa de una carta en juego a un formato string.
; Dom: ipc (in-play-card)
; Rec: string
; Tipo recursión: No aplica

(define (ipc->string ipc)
  (let ((c (ipc-card ipc)))
    (let ((nombre (nombre-a-string (card-name c))))
      (string-append 
       nombre
       (info-ex c)
       " | " 
       (symbol->string (card-pokemon-type c))
       (info-etapa c)
       (info-ps ipc c)
       (info-dano ipc)
       (info-energias ipc)
       (info-estado ipc)
       (info-debilidad c)
       (info-resistencia c)))))


;;  Funciones auxiliares

; Descripción: Utilidad para convertir el nombre de una carta (symbol o string) a string.
; Dom: n 
; Rec: string
; Tipo recursión: No aplica

(define (nombre-a-string n)
  (if (symbol? n) 
      (symbol->string n) 
      n))
; Descripción: Genera un string indicando si la carta es de tipo EX.
; Dom: c (card)
; Rec: string
; Tipo recursión: No aplica

(define (info-ex c)
 
  (if (card-is-ex? c) 
      " EX" 
      ""))

; Descripción: Genera un string con la etapa evolutiva del Pokémon.
; Dom: c (card)
; Rec: string
; Tipo recursión: No aplica

(define (info-etapa c)
  (if (card-basic-pokemon? c)
      " | Etapa: Básico"
      (string-append " | Etapa: Evolución de " (card-evolves-from c))))

; Descripción: Genera un string con la vida actual vs la vida total del Pokemon.
; Dom: ipc (in-play-card) X c (card)
; Rec: string
; Tipo recursión: No aplica

(define (info-ps ipc c)
  (string-append " | PS: " 
                 (number->string (ipc-current-hp ipc)) 
                 "/" 
                 (number->string (card-hp c))))

; Descripción: Genera un string mostrando el daño acumulado en el Pokémon.
; Dom: ipc (in-play-card)
; Rec: string
; Tipo recursión: No aplica

(define (info-dano ipc)
  (string-append " | Daño acumulado: " 
                 (number->string (ipc-damage ipc)) 
                 " PS"))
; Descripción: Genera un string con las energías unidas a la carta.
; Dom: ipc (in-play-card)
; Rec: string
; Tipo recursión: No aplica

(define (info-energias ipc)
  (string-append " | Energías: " 
                 (energies->string (ipc-energies ipc) "")))

; Descripción: Genera un string indicando el estado alterado actual del Pokémon.
; Dom: ipc (in-play-card)
; Rec: string
; Tipo recursión: No aplica

(define (info-estado ipc)
  (string-append " | Estado: " (ipc-status ipc)))

; Descripción: Genera un string indicando la debilidad de la carta original.
; Dom: c (card)
; Rec: string
; Tipo recursión: No aplica

(define (info-debilidad c)
  (if (null? (card-weakness c))
      ""
      (string-append " | Deb: " (symbol->string (card-weakness c)))))

; Descripción: Genera un string indicando la resistencia de la carta original.
; Dom: c 
; Rec: string
; Tipo recursión: No aplica

(define (info-resistencia c)
  (if (null? (card-resistance c))
      ""
      (string-append " | Res: " (symbol->string (card-resistance c)))))


;TDA PLAYER

; CONSTRUCTOR

; Descripción: Constructor principal  del TDA player.
; Dom: id (integer) X mano (lista) X banca (lista) X pokemon-activo (in-play-card/) X premios (lista) X descarte (lista) X mazo (deck)
; Rec: player (lista)
; Tipo recursión: No aplica

(define make-player
  (lambda (id mano banca pokemon-activo premios descarte mazo)
    (list id mano banca pokemon-activo premios descarte mazo #f #f)))

;PERTENENCIA

; Descripción: Función de pertenencia del jugador.
; Dom: p
; Rec: booleano
; Tipo recursión: No aplica
(define player?
  (lambda (p)
    (and (list? p)
         (= (length p) 9)
         (integer? (list-ref p 0))
         (list? (list-ref p 1))
         (list? (list-ref p 2))
         (list? (list-ref p 4))
         (list? (list-ref p 5))
         (deck? (list-ref p 6))
         (boolean? (list-ref p 7))
         (boolean? (list-ref p 8)))))



;SELECTORES


; Descripción: obtiene el ID del jugador.
; Dom: p (player)
; Rec: integer
; Tipo recursión: No aplica
(define player-id
  (lambda (p)
    (list-ref p 0)))

; Descripción: Selector que obtiene la lista de cartas en la mano del jugador.
; Dom: p 
; Rec: lista
; Tipo recursión: No aplica
(define player-hand
  (lambda (p)
    (list-ref p 1)))

; Descripción: Selector que obtiene la lista de in-play-cards en la banca del jugador.
; Dom: p (player)
; Rec: list
; Tipo recursión: No aplica
(define player-bench
  (lambda (p)
    (list-ref p 2)))

; Descripción: Selector que obtiene el Pokemon activo actual del jugador.
; Dom: p 
; Rec: in-play-card
; Tipo recursión: No aplica

(define player-active
  (lambda (p)
    (list-ref p 3)))

; Descripción: Selector que obtiene las cartas de premio del jugador.
; Dom: player
; Rec: list
; Tipo recursión: No aplica
(define player-prizes
  (lambda (p)
    (list-ref p 4)))


; Descripción: Selector que obtiene la pila de descarte del jugador.
; Dom: p (player)
; Rec: list
; Tipo recursión: No aplica
(define player-discard
  (lambda (p)
    (list-ref p 5)))

; Descripción: Selector que obtiene el mazo del jugador.
; Dom: p (player)
; Rec: deck
; Tipo recursión: No aplica
(define player-deck
  (lambda (p)
    (list-ref p 6)))

; Descripción: Selector que indica si el jugador ya usó una carta de energía en su turno.
; Dom: p (player)
; Rec: booleano
; Tipo recursión: No aplica
(define player-energy-used?
  (lambda (p)
    (list-ref p 7)))

; Descripción: Selector que indica si el jugador ya usó una carta de soporte en su turno.
; Dom: p (player)
; Rec: boolean
; Tipo recursión: No aplica
(define player-supporter-used?
  (lambda (p)
    (list-ref p 8)))


;MODIFICADORES

; Descripción: Modificador base que reconstruye la lista del jugador actualizando un índice específico.
; Dom: p (player) X idx (integer) X valor (any)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-set-field
  (lambda (p idx valor)
    (define (rebuild lst i)
      (cond
        ((null? lst) '())
        ((= i idx) (cons valor (cdr lst)))
        (else (cons (car lst) (rebuild (cdr lst) (+ i 1))))))
    (rebuild p 0)))

; Descripción: Modificador que actualiza la mano del jugador.
; Dom: p (player) X mano (lista)
; Rec: player (lista)
; Tipo recursión: No aplica
(define player-set-hand
  (lambda (p mano)
    (player-set-field p 1 mano)))

; Descripción: Modificador que actualiza la banca del jugador.
; Dom: p (player) X banca (lisat)
; Rec: player (lista)
; Tipo recursión: No aplica
(define player-set-bench
  (lambda (p banca)
    (player-set-field p 2 banca)))

; Descripción: Modificador que establece el Pokémon activo del jugador.
; Dom: p (player) X activo (in-play-card/null)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-set-active
  (lambda (p activo)
    (player-set-field p 3 activo)))

; Descripción: Modificador que actualiza las cartas de premio del jugador.
; Dom: p (player) X premios (lista)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-set-prizes
  (lambda (p premios)
    (player-set-field p 4 premios)))

; Descripción: Modificador que actualiza las cartas de descarte del jugador.
; Dom: p (player) X descarte (list)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-set-discard
  (lambda (p descarte)
    (player-set-field p 5 descarte)))

; Descripción: Modificador que actualiza el mazo del jugador.
; Dom: p (player) X mazo (deck)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-set-deck
  (lambda (p mazo)
    (player-set-field p 6 mazo)))

; Descripción: Modificador que marca si se ha usado energía en el turno.
; Dom: p (player) X usado (booleano)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-set-energy-used
  (lambda (p usado)
    (player-set-field p 7 usado)))

; Descripción: Modificador que marca si se ha usado carta de soporte en el turno.
; Dom: p (player) X usado (booleano)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-set-supporter-used
  (lambda (p usado)
    (player-set-field p 8 usado)))

;; OTRAS FUNCIONES

; Descripción: Modificador que agrega una carta a la mano del jugador.
; Dom: p (player) X c (card)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-add-to-hand
  (lambda (p c)
    (player-set-hand p (cons c (player-hand p)))))

; Descripción: Modificador que elimina una carta específica de la mano buscando por nombre.
; Dom: p (player) X nombre-carta (string/symbol)
; Rec: player (list)
; Tipo recursión: No aplica
(define (player-remove-from-hand p nombre-carta)
  (let ((nombre-str (convertir-a-string nombre-carta)))
    (let ((nueva-mano (quitar-primera-carta (player-hand p) nombre-str)))
      (player-set-hand p nueva-mano))))

; Descripción: Para asegurar que el valor evaluado sea de tipo string.
; Dom: valor 
; Rec: string/any
; Tipo recursión: No aplica
(define (convertir-a-string valor)
  (if (symbol? valor)
      (symbol->string valor)
      valor))

; Descripción: Función recursiva para remover la primera carta de la mano que coincida con el nombre.
; Dom: lista (list) X nombre-buscado (string)
; Rec: list
; Tipo recursión: Natural
(define (quitar-primera-carta lista nombre-buscado)
  (if (null? lista)
      '()
      (let ((nombre-actual (convertir-a-string (card-name (car lista)))))
        (if (string-ci=? nombre-actual nombre-buscado)
            
            (cdr lista)
            
            (cons (car lista) (quitar-primera-carta (cdr lista) nombre-buscado))))))

; Descripción: Modificador que agrega un Pokémon a la banca y valida el límite máximo.
; Dom: p (player) X ipc (in-play-card)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-add-to-bench
  (lambda (p ipc)
    (if (player-bench-full? p)
        (error "player-add-to-bench: (max 5 Pokémon)")
        (player-set-bench p (cons ipc (player-bench p))))))

; Descripción: Modificador que remueve un Pokémon de la banca buscando por su ID único.
; Dom: p (player) X id-unico (integer)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-remove-from-bench
  (lambda (p id-unico)
    (define (remove-by-id lst id)
      (cond
        ((null? lst) '())
        ((= (ipc-id (car lst)) id) (cdr lst))
        (else (cons (car lst) (remove-by-id (cdr lst) id)))))
    (player-set-bench p (remove-by-id (player-bench p) id-unico))))

; Descripción: Modificador que envía una carta a la pila de descarte del jugador.
; Dom: p (player) X c (card)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-add-to-discard
  (lambda (p c)
    (player-set-discard p (cons c (player-discard p)))))

; Descripción: Verifica si la banca del jugador ha llegado a su límite de 5 espacios.
; Dom: p (player)
; Rec: booleano
; Tipo recursión: No aplica
(define player-bench-full?
  (lambda (p)
    (>= (length (player-bench p)) 5)))

; Descripción: Verifica buscando en la mano si el jugador posee algún Pokemon en etapa basica.
; Dom: p (player)
; Rec: booleano
; Tipo recursión: No aplica
(define player-has-basics?
  (lambda (p)
    (define (buscar mano)
      (cond
        ((null? mano) #f)
        ((card-basic-pokemon? (car mano)) #t)
        (else (buscar (cdr mano)))))
    (buscar (player-hand p))))

; Descripción: Reinicia las banderas lógicas de acciones permitidas al inicio del turno.
; Dom: p (player)
; Rec: player (list)
; Tipo recursión: No aplica
(define player-reset-turn-flags
  (lambda (p)
    (player-set-supporter-used
     (player-set-energy-used p #f)
     #f)))

; Descripción: Incrementa el turno en juego de todos los Pokémon de la banca y del activo.
; Dom: p (player)
; Rec: player (list)
; Tipo recursión: No aplica

(define (player-increment-bench-turns p)
  (let ((nueva-banca (incrementar-toda-la-banca (player-bench p))))
    (let ((nuevo-activo (incrementar-si-existe (player-active p))))
      (let ((p-con-nueva-banca (player-set-bench p nueva-banca)))
        (player-set-active p-con-nueva-banca nuevo-activo)))))

; Descripción: Itera y aplica el incremento de turnos a todos los elementos en la banca.
; Dom: banca (list)
; Rec: list
; Tipo recursión: Natural
(define (incrementar-toda-la-banca banca)
  (if (null? banca)
      '()
      (cons (ipc-add-turn (car banca)) 
            (incrementar-toda-la-banca (cdr banca)))))

; Descripción: Incrementa el turno del Pokemon activo si es que existe en juego.
; Dom: activo (in-play-card/null/list)
; Rec: in-play-card/null
; Tipo recursión: No aplica
(define (incrementar-si-existe activo)
  (if (null? activo)
      null
      (ipc-add-turn activo)))

;; STRING

; Descripción: Formatea los elementos de la banca del jugador a un string enumerado.
; Dom: banca (list) X i (integer) X acc (string)
; Rec: string
; Tipo recursión: Cola
(define bench->string
  (lambda (banca i acc)
    (cond
      ((null? banca) acc)
      (else
       (bench->string
        (cdr banca)
        (+ i 1)
        (string-append acc
                       "    " (number->string i) ". "
                       (ipc->string (car banca)) "\n"))))))
; Descripción: Formatea la mano a un string detallado (o oculto) dependiendo del estado del juego.
; Dom: mano (list) X i (integer) X acc (string) X mostrar-detalle (boolean)
; Rec: string
(define hand->string
  (lambda (mano i acc mostrar-detalle)
    (cond
      ((null? mano) acc)
      (else
       (hand->string
        (cdr mano)
        (+ i 1)
        (string-append acc
                       "    " (number->string i) ". "
                       (if mostrar-detalle
                           (string-append
                            (let ((n (card-name (car mano))))
                              (if (symbol? n) (symbol->string n) n))
                            " ["
                            (symbol->string (card-type (car mano)))
                            "]\n")
                           "???\n"))
        mostrar-detalle)))))

; Descripción: Formatea las cartas de la pila de descarte a un string.
; Dom: descarte (list) X acc (string)
; Rec: string
; Tipo recursión: Cola
(define discard->string
  (lambda (descarte acc)
    (cond
      ((null? descarte) (if (string=? acc "") "    (vacío)\n" acc))
      (else
       (let ((n (card-name (car descarte))))
         (discard->string
          (cdr descarte)
          (string-append acc "    - "
                         (if (symbol? n) (symbol->string n) n)
                         "\n")))))))

; Descripción: Formatea al jugador entero en un string representativo para ser visualizado
; Dom: p (player) X mostrar-mano (booleano)
; Rec: string
; Tipo recursión: No aplica
(define player->string
  (lambda (p mostrar-mano)
    (string-append
     "  Mazo           : " (number->string (deck-size (player-deck p))) " cartas\n"
     "  Premios        : " (number->string (length (player-prizes p))) " cartas\n"
     "  Pokémon activo :\n"
     (if (null? (player-active p))
         "    (ninguno)\n"
         (string-append "    " (ipc->string (player-active p)) "\n"))
     "  Banca ("
     (number->string (length (player-bench p)))
     "/5):\n"
     (if (null? (player-bench p))
         "    (vacía)\n"
         (bench->string (player-bench p) 1 ""))
     "  Mano ("
     (number->string (length (player-hand p)))
     " cartas):\n"
     (if mostrar-mano
         (hand->string (player-hand p) 1 "" #t)
         (string-append "    ("
                        (number->string (length (player-hand p)))
                        " cartas ocultas)\n"))
     "  Descarte:\n"
     (discard->string (player-discard p) ""))))
