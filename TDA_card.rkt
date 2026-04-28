#lang racket




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