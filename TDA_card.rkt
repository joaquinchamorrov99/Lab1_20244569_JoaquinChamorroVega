#lang racket


(require "TDA_attack.rkt")


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
  (if (not (= (length args) 2))
      (error "card energy: solo requiere tipo y nombre")
      (list 'energy (cadr args))))

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
          ((not (valid-element? tipo)) (error "tipo inválido"))
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
        (else 'colorless)))))

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

