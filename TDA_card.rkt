#lang racket


(require "TDA_attack.rkt")



(provide
  card
  card?
  card-pokemon?
  card-energy?
  card-trainer?
  card-type
  card-name
  CARD-TYPE
  ELEMENT-TYPE
  ENERGY
  card-type?
  element-type?

  ; selectores pokemon
  card-evolves-from
  card-hp
  card-pokemon-type
  card-weakness
  card-resistance
  card-retreat-cost
  card-is-ex?
  card-ability
  card-attacks

  ; selectores energy
  card-energy-type

  ; selectores trainer
  card-trainer-type
  card-trainer-text
  card-trainer-function

  ; otras funciones
  card->string
  card-has-attack?
  card-get-attack
  card-basic-pokemon?)



;TDA CARD
;; REPRESENTACIÓN:
;; Una carta se representa como una lista cuyo primer elemento
;; es el tipo de carta (símbolo). Según el tipo, la estructura varía:
;;
;; Carta Pokémon:
;; (list 'pokemon nombre evolucion-de ps tipo-pokemon
;;        debilidad resistencia coste-retirada es-ex habilidad ataques)
;;
;; Carta Energía:
;; (list 'energy nombre)
;;
;; Carta Entrenador:
;; (list 'trainer nombre trainer-type texto funcion)
; Tipos de carta como símbolos


(define CARD-TYPE '(pokemon energy trainer))
(define TRAINER-TYPES '("partidario" "objeto"))

(define ELEMENT-TYPE
  '(grass fire water lightning psychic fighting darkness metal colorless fairy))


; Descripción: Verifica si un símbolo dado corresponde a un tipo de carta principal válido ('pokemon, 'energy, o 'trainer).
; Dom: t (symbol)
; Rec: boolean
; Tipo recursión: No aplica
(define (card-type? t)
  (and (symbol? t) (member t CARD-TYPE) #t))
;Descripción: Verifica si un símbolo dado corresponde a un tipo de elemento válido.
; Dom: e (symbol)
; Rec: boolean
; Tipo recursión: No aplica
(define (element-type? e)
  (and (symbol? e) (member e ELEMENT-TYPE) #t))

(define ENERGY
  '((fire-energy      . fire)
    (water-energy     . water)
    (grass-energy     . grass)
    (lightning-energy . lightning)
    (psychic-energy   . psychic)
    (fighting-energy  . fighting)
    (darkness-energy  . darkness)
    (metal-energy     . metal)
    (fairy-energy     . fairy)
    (colorless-energy . colorless)))

; Descripción: Valida si un elemento es correcto,  apoyándose en la lista ELEMENT-TYPE.
; Dom: e (symbol/string/any)
; Rec: boolean
; Tipo recursión: No aplica
(define (valid-element? e)
  (if (symbol? e)
      (esta-en-lista? e ELEMENT-TYPE)
      (if (string? e)
          (esta-en-lista? (string->symbol (string-downcase e)) ELEMENT-TYPE)
          #f)))
; Descripción: Busca recursivamente si un ítem específico se encuentra dentro de una lista '.
; Dom: item (symbol) X lista (list)
; Rec: booleano
; Tipo recursión: Cola
(define (esta-en-lista? item lista)
  (if (null? lista)
      #f
      (if (eq? item (car lista))
          #t
          (esta-en-lista? item (cdr lista)))))
; Descripción: Valida si un tipo de carta es correcto, aceptando símbolos o strings, revisando contra CARD-TYPE.
; Dom: t (symbol/string/any)
; Rec: boolean
; Tipo recursión: No aplica

(define (valid-card-type? t)
  (if (symbol? t)
      (esta-en-lista2? t CARD-TYPE)
      (if (string? t)
          (esta-en-lista2? (string->symbol (string-downcase t)) CARD-TYPE)
          #f)))
; Descripción: Busca recursivamente si un elemento está en una lista usando .
; Dom: elemento (symbol) X lista (list)
; Rec: boolean
; Tipo recursión: Cola
(define (esta-en-lista2? elemento lista)
  (if (null? lista)
      #f
      (if (eq? elemento (car lista))
          #t
          (esta-en-lista2? elemento (cdr lista)))))
; Descripción: Verifica si un string corresponde a un tipo de entrenador válido .
; Dom: t (string)
; Rec: boolean
; Tipo recursión: No aplica
(define (valid-trainer-type? t)
  (if (string? t)
      (esta-en-lista3? (string-downcase t) TRAINER-TYPES)
      #f))
; Descripción: Busca recursivamente si un string está en una lista .
; Dom: elemento (string) X lista (list)
; Rec: boolean
; Tipo recursión: Cola
(define (esta-en-lista3? elemento lista)
  (if (null? lista)
      #f
      (if (string=? elemento (car lista))
          #t
          (esta-en-lista3? elemento (cdr lista)))))
; Descripción: Convierte de forma segura un valor a símbolo.
; Dom: v (symbol/string)
; Rec: symbol
; Tipo recursión: No aplic
(define (to-symbol v)
  (if (symbol? v)
      v
      (if (string? v)
          (string->symbol (string-downcase v))
          v)))



; Descripción: Verifica si un valor, convertido a símbolo, coincide con un símbolo esperado.
; Dom: v (any) X expected-sym (symbol)
; Rec: boolean
; Tipo recursión: No aplica
(define (is-card-type? v expected-sym)
  (if (eq? (to-symbol v) expected-sym)
      #t
      #f))

; Descripción: Valida una lista de ataques asegurando que cumplan los límites de cantidad según si el Pokémon posee o no una habilidad.
; Dom: ataques (list) X tiene-habilidad (boolean)
; Rec: boolean
; Tipo recursión: Cola
(define valid-attacks?
  (lambda (ataques tiene-habilidad)
    (cond
      ((not (list? ataques)) #f)
      ((and tiene-habilidad (> (length ataques) 2)) #f)
      ((> (length ataques) 3) #f)
      ((null? ataques) #t)
      ((not (attack? (car ataques))) #f)
      (else (valid-attacks? (cdr ataques) tiene-habilidad)))))

;; CONSTRUCTOR

; Descripción: Constructor principal  del TDA card.
; Dom: args 
; Rec: card (list)
; Tipo recursión: No aplica
(define (card . args)
  (if (< (length args) 2)
      (error "card:  requiere al menos 2 argumentos")
      (let ((tipo (car args)))
        (cond
          ((is-card-type? tipo 'energy)  (crear-carta-energia args))
          ((is-card-type? tipo 'trainer) (crear-carta-entrenador args))
          ((is-card-type? tipo 'pokemon) (crear-carta-pokemon args))
          (else (error "card: tipoCarta inválido"))))))
;; --- Sub-función: Energía ---
;funcion auxiliar
; Descripción: Determina el símbolo correcto para un tipo de energía a partir de un valor dado.
; Dom: v (any)
; Rec: symbol/boolean
; Tipo recursión: No aplic
(define (resolver-tipo-energia v)
  (cond
    ((valid-element? v) (to-symbol v))
    ((and (symbol? v) (assq v ENERGY)) (cdr (assq v ENERGY)))
    (else #f)))

; Descripción: constructor dedicado a estructurar una carta de energía validando sus componentes.
; Dom: args (list)
; Rec: card (list)
; Tipo recursión: No aplica
(define (crear-carta-energia args)
  (cond
    ((= (length args) 2)
     (list 'energy (cadr args)))
    ((= (length args) 3)
     (let ((nombre (cadr args))
           (tipo-e (caddr args)))
       (cond
         ((not (or (string? nombre) (symbol? nombre)))
          (error "card energy: nombre debe ser string o symbol"))
         (else
          (let ((tipo-resuelto (resolver-tipo-energia tipo-e)))
            (cond
              ((not tipo-resuelto)
               (error "card energy: tipo de energía no vallido" tipo-e))
              (else (list 'energy nombre tipo-resuelto))))))))
    (else (error "card energy: requiere 2 o 3 argumentos"
                 (length args)))))

;; --- Sub-función: Entrenador ---
; Descripción: constructor dedicado a estructurar una carta de entrenador validando su nombre, tipo, texto y función de efecto.
; Dom: args (list)
; Rec: card (list)
; Tipo recursión: No aplica
(define (crear-carta-entrenador args)
  (if (not (= (length args) 5))
      (error "card trainer: requiere 5 argumentos")
      (let ((nombre (list-ref args 1))
            (t-tipo (list-ref args 2))
            (texto  (list-ref args 3))
            (func   (list-ref args 4)))
        (if (not (string? nombre)) (error "nombre debe ser string")
        (if (not (valid-trainer-type? t-tipo)) (error "tipo inválido")
        (if (not (string? texto)) (error "texto debe ser string")
        (if (not (procedure? func)) (error "procedimiento no valido")
            (list 'trainer nombre (string-downcase t-tipo) texto func))))))))
;; --- Sub-función: Pokémon ---
; Descripción: Sub-constructor dedicado a estructurar una carta de Pokémon validando exhaustivamente todos sus atributos (PS, tipos, ataques, etc.).
; Dom: args (list)
; Rec: card (list)
; Tipo recursión: No aplica
(define (crear-carta-pokemon args)
  (if (not (= (length args) 11))
      (error "card pokemon: requiere 11 argumentos")
      (let ((n (list-ref args 1)) (ev (list-ref args 2)) (ps (list-ref args 3))
            (tipo (list-ref args 4)) (deb (list-ref args 5)) (res (list-ref args 6))
            (ret (list-ref args 7)) (ex (list-ref args 8)) (hab (list-ref args 9))
            (atq (list-ref args 10)))
        
        (cond
          ((not (string? n)) (error "nombre no valido"))
          ((not (integer? ps)) (error "ps debe ser entero"))
          ((not (positive? ps)) (error "ps debe ser positivo, no 0 ni negativo" ps))
          ((not (valid-element? tipo)) (error "tipo no valido"))
          ((not (valid-attacks? atq (not (null? hab))))
           (error "ataques: máximo 3, o máximo 2 si el pokémon tiene habilidad"))
          (else (list 'pokemon n ev ps (to-symbol tipo) 
                      (if (null? deb) null (to-symbol deb))
                      (if (null? res) null (to-symbol res))
                      ret ex hab atq))))))


     
      
;; FUNCIONES DE PERTENENCIA

; Descripción: Función principal de pertenencia que verifica que un elemento posea la estructura  de una carta.
; Dom: c (any)
; Rec: boolean
; Tipo recursión: No aplica
(define card?
  (lambda (c)
    (and (list? c)
         (>= (length c) 2)
         (symbol? (car c))
         (member (car c) '(pokemon energy trainer))
         #t)))
; Descripción:  verifica si la carta es del subtipo Pokémon.
; Dom: c (any
; Rec: boolean
; Tipo recursión: No aplica
(define card-pokemon?
  (lambda (c)
    (and (card? c)
         (eq? (car c) 'pokemon))))
; Descripción: verifica si una carta es del subtipo Energía.
; Dom: c (any)
; Rec: boolean
; Tipo recursión: No aplica
(define card-energy?
  (lambda (c)
    (and (card? c)
         (eq? (car c) 'energy))))
; Descripción:  verifica si una carta es del subtipo Entrenador.
; Dom: c (any)
; Rec: boolean
; Tipo recursión: No aplica
(define card-trainer?
  (lambda (c)
    (and (card? c)
         (eq? (car c) 'trainer))))

; Descripción: Función de pertenencia que verifica si una carta Pokémon esta en etapa basica).
; Dom: c (any)
; Rec: boolean
; Tipo recursión: No aplica
(define card-basic-pokemon?
  (lambda (c)
    (and (card-pokemon? c)
         (null? (card-evolves-from c)))))


;SELECTORES

; Descripción: Selector que obtiene el tipo  de  carta ('pokemon, 'energy o 'trainer).
; Dom: c (card)
; Rec: symbol
; Tipo recursión: No aplica

(define card-type
  (lambda (c)
    (list-ref c 0)))
; Descripción: Selector que obtiene el nombre de la carta.
; Dom: c (card)
; Rec: string/symbol
; Tipo recursión: No aplica
(define card-name
  (lambda (c)
    (list-ref c 1)))
;selectores
;Descripción: Selector natural que obtiene el nombre del Pokémon del cual evoluciona la carta.
; Dom: c (card)
; Rec: string/null
; Tipo recursión: No aplica
(define card-evolves-from
  (lambda (c)
    (list-ref c 2)))
;Descripción: Selector que obtiene los hp de un pokemn.
; Dom: c (card)
; Rec: integer
; Tipo recursión: No aplica
(define card-hp
  (lambda (c)
    (list-ref c 3)))
; Descripción: Selector  que obtiene el tipo elemental del pokemon .
; Dom: c (card)
; Rec: symbol
; Tipo recursión: No aplica
(define card-pokemon-type
  (lambda (c)
    (list-ref c 4)))
; Descripción: Selector  que obtiene el elemento al que es debil el pokemon.
; Dom: c (card)
; Rec: symbol
; Tipo recursión: No aplica
(define card-weakness
  (lambda (c)
    (list-ref c 5)))
; Descripción: Selector que obtiene el tipo elemental al que es resistente el pokemon.
; Dom: c (card)
; Rec: symbol/null
; Tipo recursión: No aplica
(define card-resistance
  (lambda (c)
    (list-ref c 6)))
; Descripción: Selector  que obtiene el coste de energías incoloras requeridas para retirar al Pokemon.
; Dom: c (card)
; Rec: integer/null
; Tipo recursión: No aplica
(define card-retreat-cost
  (lambda (c)
    (list-ref c 7)))
; Descripción: Selector que indica si la carta es una variante  "EX".
; Dom: c (card)
; Rec: boolean
; Tipo recursión: No aplica
(define card-is-ex?
  (lambda (c)
    (list-ref c 8)))
; Descripción: Selector  que obtiene la habilidad especial del Pokémon, si la posee.
; Dom: c (card)
; Rec: list/null
; Tipo recursión: No aplica
(define card-ability
  (lambda (c)
    (list-ref c 9)))

; Descripción: Selector  que obtiene la lista de ataques del Pokemon.
; Dom: c (card)
; Rec: list
; Tipo recursión: No aplica
(define card-attacks
  (lambda (c)
    (list-ref c 10)))

; selector energia
; Descripción: Selector  que determina el tipo elemental que provee una carta .
; Dom: c (card)
; Rec: symbol
; Tipo recursión: No aplica
(define card-energy-type
  (lambda (c)
    (cond
      
      ((>= (length c) 3) (list-ref c 2))
      
      (else
       (let ((nombre (card-name c)))
         (cond
           ((symbol? nombre)
            
            (let ((par (assq nombre ENERGY)))
              (cond
                (par (cdr par))
                (else
                 (let ((s (symbol->string nombre)))
                   (cond
                     ((regexp-match? #rx"-energy$" s)
                      (string->symbol (substring s 0 (- (string-length s) 7))))
                     (else nombre)))))))
           ((string? nombre)
            (let ((sym (string->symbol (string-downcase nombre))))
              (let ((par (assq sym ENERGY)))
                (cond
                  (par (cdr par))
                  (else
                   (let ((n (string-downcase nombre)))
                     (cond
                       ((regexp-match? #rx"-energy$" n)
                        (string->symbol (substring n 0 (- (string-length n) 7))))
                       (else 'colorless))))))))
           (else 'colorless)))))))


;selectores entrenador

; Descripción: Selector  que obtiene el subtipo de la carta de entrenador .
; Dom: c (card)
; Rec: string
; Tipo recursión: No aplica
(define card-trainer-type
  (lambda (c)
    (list-ref c 2)))

; Descripción: Selector  que obtiene el texto  del efecto del entrenador.
; Dom: c (card)
; Rec: string
; Tipo recursión: No aplica
(define card-trainer-text
  (lambda (c)
    (list-ref c 3)))


; Descripción: Selector  que obtiene la funcion asociada al efecto de la carta de entrenador.
; Dom: c (card)
; Rec: procedure
; Tipo recursión: No aplica
(define card-trainer-function
  (lambda (c)
    (list-ref c 4)))


;OTRAS FUNCIONES

; Descripción: Verifica buscando en la lista de ataques del Pokemon si este posee un ataque con un nombre en específico.
; Dom: c (card) X nombre-ataque (string/symbol)
; Rec: boolean
; Tipo recursión: No aplica
(define card-has-attack?
  (lambda (c nombre-ataque)
    (define (buscar ataques)
      (cond
        ((null? ataques) #f)
        ((attack-has-name? (car ataques) nombre-ataque) #t)
        (else (buscar (cdr ataques)))))
    (and (card-pokemon? c)
         (buscar (card-attacks c)))))

; Descripción: Busca y retorna la estructura de un ataque específico del Pokémon según su nombre.
; Dom: c (card) X nombre-ataque (string/symbol)
; Rec: attack (list) / boolean

(define card-get-attack
  (lambda (c nombre-ataque)
    (define (buscar ataques)
      (cond
        ((null? ataques) #f)
        ((attack-has-name? (car ataques) nombre-ataque) (car ataques))
        (else (buscar (cdr ataques)))))
    (and (card-pokemon? c)
         (buscar (card-attacks c)))))
; Descripción: Itera una lista de ataques concatenando la representación en string de cada uno.
; Dom: ataques (list) X acc (string)
; Rec: string
; Tipo recursión: Cola
(define attacks->string
  (lambda (ataques acc)
    (cond
      ((null? ataques) acc)
      (else
       (attacks->string
        (cdr ataques)
        (string-append acc (attack->string (car ataques))))))))

; Descripción: Genera una representación en formato de texto detallado de una carta dependiendo de su subtipo específico.
; Dom: c (card)
; Rec: string
; Tipo recursión: No aplica
(define (card->string c)
  (if (card-energy? c)
      (string-append "[ENERGIA] " (nombre-a-string (card-name c)) "\n")
      (if (card-trainer? c)
          (string-append "[ENTRENADOR - " (string-upcase (card-trainer-type c)) "] "
                         (card-name c) "\n  Efecto: " (card-trainer-text c) "\n")
          (if (card-pokemon? c)
              (string-append "[POKEMON] " (card-name c) (if (card-is-ex? c) " EX" "") "\n"
                             (info-etapa c)
                             "  PS        : " (number->string (card-hp c)) "\n"
                             "  Tipo      : " (symbol->string (card-pokemon-type c)) "\n"
                             (info-debilidad c)
                             (info-resistencia c)
                             (info-retirada c))
              "Carta desconocida\n"))))

; Descripción: Asegura que el nombre retornado sea string.
; Dom: n (symbol/string)
; Rec: string
; Tipo recursión: No aplica

(define (nombre-a-string n)
  (if (symbol? n) (symbol->string n) n))
; Descripción: Formatea la información de evolución de un Pokémon para impresión.
; Dom: c (card)
; Rec: string
; Tipo recursión: No aplica
(define (info-etapa c)
  (if (card-basic-pokemon? c)
      "  Etapa     : Basico\n"
      (string-append "  Etapa     : Evoluciona de " (card-evolves-from c) "\n")))

(define (info-debilidad c)
  (string-append "  Debilidad : " (if (null? (card-weakness c)) "ninguna" (symbol->string (card-weakness c))) "\n"))
; Descripción: Formatea la información de resistencia de un Pokémon para impresión.
; Dom: card
; Rec: string
; Tipo recursión: No aplica

(define (info-resistencia c)
  (string-append "  Resistencia: " (if (null? (card-resistance c)) "ninguna" (symbol->string (card-resistance c))) "\n"))
; Descripción: Formatea la información de coste de retirada de un Pokémon para impresion.
; Dom: c (card)
; Rec: string
; Tipo recursión: No aplica

(define (info-retirada c)
  (if (null? (card-retreat-cost c))
      "  Retirada  : ninguna\n"
      (string-append "  Retirada  : " (number->string (card-retreat-cost c)) " energía(s) incolora(s)\n")))


