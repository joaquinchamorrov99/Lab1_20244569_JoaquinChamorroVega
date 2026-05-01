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


; Tipos de carta como símbolos
(define CARD-TYPE '(pokemon energy trainer))
(define TRAINER-TYPES '("partidario" "objeto"))

(define ELEMENT-TYPE
  '(grass fire water lightning psychic fighting darkness metal colorless fairy))


(define (valid-element? e)
  (if (symbol? e)
      (esta-en-lista? e ELEMENT-TYPE)
      (if (string? e)
          (esta-en-lista? (string->symbol (string-downcase e)) ELEMENT-TYPE)
          #f)))

(define (esta-en-lista? item lista)
  (if (null? lista)
      #f
      (if (eq? item (car lista))
          #t
          (esta-en-lista? item (cdr lista)))))


(define (valid-card-type? t)
  (if (symbol? t)
      (esta-en-lista2? t CARD-TYPE)
      (if (string? t)
          (esta-en-lista2? (string->symbol (string-downcase t)) CARD-TYPE)
          #f)))

(define (esta-en-lista2? elemento lista)
  (if (null? lista)
      #f
      (if (eq? elemento (car lista))
          #t
          (esta-en-lista2? elemento (cdr lista)))))

(define (valid-trainer-type? t)
  (if (string? t)
      (esta-en-lista3? (string-downcase t) TRAINER-TYPES)
      #f))

(define (esta-en-lista3? elemento lista)
  (if (null? lista)
      #f
      (if (string=? elemento (car lista))
          #t
          (esta-en-lista3? elemento (cdr lista)))))

(define (to-symbol v)
  (if (symbol? v)
      v
      (if (string? v)
          (string->symbol (string-downcase v))
          v)))




(define (is-card-type? v expected-sym)
  (if (eq? (to-symbol v) expected-sym)
      #t
      #f))


(define valid-attacks?
  (lambda (ataques tiene-habilidad)
    (cond
      ((not (list? ataques)) #f)
      ((and tiene-habilidad (> (length ataques) 2)) #f)
      ((> (length ataques) 3) #f)
      ((null? ataques) #t)
      ((not (attack? (car ataques))) #f)
      (else (valid-attacks? (cdr ataques) tiene-habilidad)))))


(define (card . args)
  (if (< (length args) 2)
      (error "card: se requieren al menos 2 argumentos")
      (let ((tipo (car args)))
        (cond
          ((is-card-type? tipo 'energy)  (crear-carta-energia args))
          ((is-card-type? tipo 'trainer) (crear-carta-entrenador args))
          ((is-card-type? tipo 'pokemon) (crear-carta-pokemon args))
          (else (error "card: tipoCarta inválido"))))))

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
         ((not (valid-element? tipo-e))
          (error "card energy: tipo de energía inválido" tipo-e))
         (else (list 'energy nombre (to-symbol tipo-e))))))
    (else (error "card energy: requiere 2 o 3 argumentos"
                 (length args)))))

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
        (if (not (procedure? func)) (error "procedimiento inválido")
            (list 'trainer nombre (string-downcase t-tipo) texto func))))))))

(define (crear-carta-pokemon args)
  (if (not (= (length args) 11))
      (error "card pokemon: requiere 11 argumentos")
      (let ((n (list-ref args 1)) (ev (list-ref args 2)) (ps (list-ref args 3))
            (tipo (list-ref args 4)) (deb (list-ref args 5)) (res (list-ref args 6))
            (ret (list-ref args 7)) (ex (list-ref args 8)) (hab (list-ref args 9))
            (atq (list-ref args 10)))
        
        (cond
          ((not (string? n)) (error "nombre inválido"))
          ((not (integer? ps)) (error "ps debe ser entero"))
          ((not (positive? ps)) (error "ps debe ser positivo, no 0 ni negativo" ps))
          ((not (valid-element? tipo)) (error "tipo inválido"))
          ((not (valid-attacks? atq (not (null? hab))))
           (error "ataques: máximo 3, o máximo 2 si el pokémon tiene habilidad"))
          (else (list 'pokemon n ev ps (to-symbol tipo) 
                      (if (null? deb) null (to-symbol deb))
                      (if (null? res) null (to-symbol res))
                      ret ex hab atq))))))


     
      
;Pertenencia
(define card?
  (lambda (c)
    (and (list? c)
         (>= (length c) 2)
         (symbol? (car c))
         (member (car c) '(pokemon energy trainer))
         #t)))

(define card-pokemon?
  (lambda (c)
    (and (card? c)
         (eq? (car c) 'pokemon))))

(define card-energy?
  (lambda (c)
    (and (card? c)
         (eq? (car c) 'energy))))

(define card-trainer?
  (lambda (c)
    (and (card? c)
         (eq? (car c) 'trainer))))


(define card-basic-pokemon?
  (lambda (c)
    (and (card-pokemon? c)
         (null? (card-evolves-from c)))))


;selectores



(define card-type
  (lambda (c)
    (list-ref c 0)))

(define card-name
  (lambda (c)
    (list-ref c 1)))
;selectores

(define card-evolves-from
  (lambda (c)
    (list-ref c 2)))

(define card-hp
  (lambda (c)
    (list-ref c 3)))

(define card-pokemon-type
  (lambda (c)
    (list-ref c 4)))

(define card-weakness
  (lambda (c)
    (list-ref c 5)))

(define card-resistance
  (lambda (c)
    (list-ref c 6)))

(define card-retreat-cost
  (lambda (c)
    (list-ref c 7)))

(define card-is-ex?
  (lambda (c)
    (list-ref c 8)))

(define card-ability
  (lambda (c)
    (list-ref c 9)))


(define card-attacks
  (lambda (c)
    (list-ref c 10)))

; selector energia
(define card-energy-type
  (lambda (c)
    (cond
      
      ((>= (length c) 3) (list-ref c 2))
      
      (else
       (let ((nombre (card-name c)))
         (cond
           ((symbol? nombre)
            (let ((s (symbol->string nombre)))
              (cond
                ((regexp-match? #rx"-energy$" s)
                 (string->symbol (substring s 0 (- (string-length s) 7))))
                (else nombre))))
           ((string? nombre)
            (let ((n (string-downcase nombre)))
              (cond
                ((regexp-match? #rx"-energy$" n)
                 (string->symbol (substring n 0 (- (string-length n) 7))))
                (else 'colorless))))
           (else 'colorless)))))))

;selectores entrenador
(define card-trainer-type
  (lambda (c)
    (list-ref c 2)))


(define card-trainer-text
  (lambda (c)
    (list-ref c 3)))



(define card-trainer-function
  (lambda (c)
    (list-ref c 4)))


;Otras funciones


(define card-has-attack?
  (lambda (c nombre-ataque)
    (define (buscar ataques)
      (cond
        ((null? ataques) #f)
        ((attack-has-name? (car ataques) nombre-ataque) #t)
        (else (buscar (cdr ataques)))))
    (and (card-pokemon? c)
         (buscar (card-attacks c)))))



(define card-get-attack
  (lambda (c nombre-ataque)
    (define (buscar ataques)
      (cond
        ((null? ataques) #f)
        ((attack-has-name? (car ataques) nombre-ataque) (car ataques))
        (else (buscar (cdr ataques)))))
    (and (card-pokemon? c)
         (buscar (card-attacks c)))))


(define attacks->string
  (lambda (ataques acc)
    (cond
      ((null? ataques) acc)
      (else
       (attacks->string
        (cdr ataques)
        (string-append acc (attack->string (car ataques))))))))


(define (card->string c)
  (if (card-energy? c)
      (string-append "[ENERGÍA] " (nombre-a-string (card-name c)) "\n")
      (if (card-trainer? c)
          (string-append "[ENTRENADOR - " (string-upcase (card-trainer-type c)) "] "
                         (card-name c) "\n  Efecto: " (card-trainer-text c) "\n")
          (if (card-pokemon? c)
              (string-append "[POKÉMON] " (card-name c) (if (card-is-ex? c) " EX" "") "\n"
                             (info-etapa c)
                             "  PS        : " (number->string (card-hp c)) "\n"
                             "  Tipo      : " (symbol->string (card-pokemon-type c)) "\n"
                             (info-debilidad c)
                             (info-resistencia c)
                             (info-retirada c))
              "Carta desconocida\n"))))



(define (nombre-a-string n)
  (if (symbol? n) (symbol->string n) n))

(define (info-etapa c)
  (if (card-basic-pokemon? c)
      "  Etapa     : Básico\n"
      (string-append "  Etapa     : Evoluciona de " (card-evolves-from c) "\n")))

(define (info-debilidad c)
  (string-append "  Debilidad : " (if (null? (card-weakness c)) "ninguna" (symbol->string (card-weakness c))) "\n"))

(define (info-resistencia c)
  (string-append "  Resistencia: " (if (null? (card-resistance c)) "ninguna" (symbol->string (card-resistance c))) "\n"))

(define (info-retirada c)
  (if (null? (card-retreat-cost c))
      "  Retirada  : ninguna\n"
      (string-append "  Retirada  : " (number->string (card-retreat-cost c)) " energía(s) incolora(s)\n")))


