#lang racket



(require "TDA_card.rkt")
(require "TDA_deck.rkt")

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



(define VALID-STATUS '("normal" "dormido" "paralizado" "confundido" "envenenado"))


(define (valid-status? s)
  (if (string? s)
      (buscar-estado? (string-downcase s) VALID-STATUS)
      #f))

(define (buscar-estado? estado lista)
  (if (null? lista)
      #f
      (if (string=? estado (car lista))
          #t
          (buscar-estado? estado (cdr lista)))))


(define make-in-play-card
  (lambda (c id-unico)
    (if (not (card-pokemon? c))
        (error "make-in-play-card: solo se pueden poner en juego cartas Pokémon")
        (list c '() 0 "normal" 0 id-unico))))


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


(define ipc-card
  (lambda (ipc)
    (list-ref ipc 0)))


(define ipc-energies
  (lambda (ipc)
    (list-ref ipc 1)))


(define ipc-damage
  (lambda (ipc)
    (list-ref ipc 2)))


(define ipc-status
  (lambda (ipc)
    (list-ref ipc 3)))


(define ipc-turns-in-play
  (lambda (ipc)
    (list-ref ipc 4)))


(define ipc-id
  (lambda (ipc)
    (list-ref ipc 5)))


(define ipc-set-field
  (lambda (ipc idx valor)
    (define (rebuild lst i)
      (cond
        ((null? lst) '())
        ((= i idx) (cons valor (cdr lst)))
        (else (cons (car lst) (rebuild (cdr lst) (+ i 1))))))
    (rebuild ipc 0)))

(define ipc-set-energies
  (lambda (ipc energias)
    (ipc-set-field ipc 1 energias)))


(define ipc-set-damage
  (lambda (ipc daño)
    (ipc-set-field ipc 2 daño)))


(define ipc-set-status
  (lambda (ipc estado)
    (if (not (valid-status? estado))
        (error "ipc-set-status: estado inválido" estado)
        (ipc-set-field ipc 3 (string-downcase estado)))))


(define ipc-add-turn
  (lambda (ipc)
    (ipc-set-field ipc 4 (+ (ipc-turns-in-play ipc) 1))))


(define ipc-add-energy
  (lambda (ipc tipo-energia)
    (ipc-set-energies ipc
                      (cons tipo-energia (ipc-energies ipc)))))


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


(define ipc-current-hp
  (lambda (ipc)
    (max 0 (- (card-hp (ipc-card ipc)) (ipc-damage ipc)))))


(define ipc-is-knocked-out?
  (lambda (ipc)
    (= (ipc-current-hp ipc) 0)))


(define ipc-energy-types
  (lambda (ipc)
    (ipc-energies ipc)))


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


(define (nombre-a-string n)
  (if (symbol? n) 
      (symbol->string n) 
      n))

(define (info-ex c)
 
  (if (card-is-ex? c) 
      " EX" 
      ""))

(define (info-etapa c)
  (if (card-basic-pokemon? c)
      " | Etapa: Básico"
      (string-append " | Etapa: Evolución de " (card-evolves-from c))))

(define (info-ps ipc c)
  (string-append " | PS: " 
                 (number->string (ipc-current-hp ipc)) 
                 "/" 
                 (number->string (card-hp c))))

(define (info-dano ipc)
  (string-append " | Daño acumulado: " 
                 (number->string (ipc-damage ipc)) 
                 " PS"))

(define (info-energias ipc)
  (string-append " | Energías: " 
                 (energies->string (ipc-energies ipc) "")))

(define (info-estado ipc)
  (string-append " | Estado: " (ipc-status ipc)))

(define (info-debilidad c)
  (if (null? (card-weakness c))
      ""
      (string-append " | Deb: " (symbol->string (card-weakness c)))))

(define (info-resistencia c)
  (if (null? (card-resistance c))
      ""
      (string-append " | Res: " (symbol->string (card-resistance c)))))



(define make-player
  (lambda (id mano banca pokemon-activo premios descarte mazo)
    (list id mano banca pokemon-activo premios descarte mazo #f #f)))


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


(define player-id
  (lambda (p)
    (list-ref p 0)))


(define player-hand
  (lambda (p)
    (list-ref p 1)))


(define player-bench
  (lambda (p)
    (list-ref p 2)))


(define player-active
  (lambda (p)
    (list-ref p 3)))


(define player-prizes
  (lambda (p)
    (list-ref p 4)))

(define player-discard
  (lambda (p)
    (list-ref p 5)))


(define player-deck
  (lambda (p)
    (list-ref p 6)))


(define player-energy-used?
  (lambda (p)
    (list-ref p 7)))


(define player-supporter-used?
  (lambda (p)
    (list-ref p 8)))


(define player-set-field
  (lambda (p idx valor)
    (define (rebuild lst i)
      (cond
        ((null? lst) '())
        ((= i idx) (cons valor (cdr lst)))
        (else (cons (car lst) (rebuild (cdr lst) (+ i 1))))))
    (rebuild p 0)))


(define player-set-hand
  (lambda (p mano)
    (player-set-field p 1 mano)))


(define player-set-bench
  (lambda (p banca)
    (player-set-field p 2 banca)))


(define player-set-active
  (lambda (p activo)
    (player-set-field p 3 activo)))


(define player-set-prizes
  (lambda (p premios)
    (player-set-field p 4 premios)))


(define player-set-discard
  (lambda (p descarte)
    (player-set-field p 5 descarte)))


(define player-set-deck
  (lambda (p mazo)
    (player-set-field p 6 mazo)))


(define player-set-energy-used
  (lambda (p usado)
    (player-set-field p 7 usado)))


(define player-set-supporter-used
  (lambda (p usado)
    (player-set-field p 8 usado)))




(define player-add-to-hand
  (lambda (p c)
    (player-set-hand p (cons c (player-hand p)))))


(define (player-remove-from-hand p nombre-carta)
  (let ((nombre-str (convertir-a-string nombre-carta)))
    (let ((nueva-mano (quitar-primera-carta (player-hand p) nombre-str)))
      (player-set-hand p nueva-mano))))


(define (convertir-a-string valor)
  (if (symbol? valor)
      (symbol->string valor)
      valor))


(define (quitar-primera-carta lista nombre-buscado)
  (if (null? lista)
      '()
      (let ((nombre-actual (convertir-a-string (card-name (car lista)))))
        (if (string-ci=? nombre-actual nombre-buscado)
            
            (cdr lista)
            
            (cons (car lista) (quitar-primera-carta (cdr lista) nombre-buscado))))))


(define player-add-to-bench
  (lambda (p ipc)
    (if (player-bench-full? p)
        (error "player-add-to-bench: la banca está llena (máx 5 Pokémon)")
        (player-set-bench p (cons ipc (player-bench p))))))


(define player-remove-from-bench
  (lambda (p id-unico)
    (define (remove-by-id lst id)
      (cond
        ((null? lst) '())
        ((= (ipc-id (car lst)) id) (cdr lst))
        (else (cons (car lst) (remove-by-id (cdr lst) id)))))
    (player-set-bench p (remove-by-id (player-bench p) id-unico))))


(define player-add-to-discard
  (lambda (p c)
    (player-set-discard p (cons c (player-discard p)))))


(define player-bench-full?
  (lambda (p)
    (>= (length (player-bench p)) 5)))


(define player-has-basics?
  (lambda (p)
    (define (buscar mano)
      (cond
        ((null? mano) #f)
        ((card-basic-pokemon? (car mano)) #t)
        (else (buscar (cdr mano)))))
    (buscar (player-hand p))))


(define player-reset-turn-flags
  (lambda (p)
    (player-set-supporter-used
     (player-set-energy-used p #f)
     #f)))


(define (player-increment-bench-turns p)
  (let ((nueva-banca (incrementar-toda-la-banca (player-bench p))))
    (let ((nuevo-activo (incrementar-si-existe (player-active p))))
      (let ((p-con-nueva-banca (player-set-bench p nueva-banca)))
        (player-set-active p-con-nueva-banca nuevo-activo)))))


(define (incrementar-toda-la-banca banca)
  (if (null? banca)
      '()
      (cons (ipc-add-turn (car banca)) 
            (incrementar-toda-la-banca (cdr banca)))))


(define (incrementar-si-existe activo)
  (if (null? activo)
      null
      (ipc-add-turn activo)))



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
