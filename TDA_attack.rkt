#lang racket


(provide
  attack
  attack?
  attack-cost
  attack-name
  attack-text
  attack-function
  attack->string
  attack-has-name?
  attack-can-use?
  elem->symbol)

;Tipo de elemento
(define ELEMENT-TYPE
  '(grass fire water lightning psychic fighting darkness metal colorless fairy))
;Funciones devalidacion
(define valid-element?
  (lambda (e)
    (if (symbol? e)
        (if (esta-en-la-lista? e ELEMENT-TYPE)
            #t
            #f)
        (if (string? e)
            #f
            #f))))

(define esta-en-la-lista?
  (lambda (elemento lista)
    (if (null? lista)
        #f
        (if (eq? elemento (car lista))
            #t
            (esta-en-la-lista? elemento (cdr lista))))))

(define all-valid-elements?
  (lambda (lst)
    (cond
      ((null? lst) #t)
      ((not (valid-element? (car lst))) #f)
      (else (all-valid-elements? (cdr lst))))))


(define elem->symbol
  (lambda (e)
    (cond
      ((symbol? e) e)
      ((string? e) (string->symbol (string-downcase e)))
      (else e))))


;Constructor
(define attack
  (lambda (costo nombre texto funcion)
    (cond
      ((not (list? costo))
       (error "attack: costo debe ser una lista de tipos de energía"))
      ((not (all-valid-elements? costo))
       (error "attack: costo contiene tipos de energía inválidos" costo))
      ((not (string? nombre))
       (error "attack: nombre debe ser un string"))
      ((not (string? texto))
       (error "attack: texto debe ser un string"))
      ((not (procedure? funcion))
       (error "attack: funcion debe ser un procedimiento"))
      (else
       
       (list (map elem->symbol costo) nombre texto funcion)))))

;Funcion de pertenencia

(define attack?
  (lambda (a)
    (and (list? a)
         (= (length a) 4)
         (list? (list-ref a 0))
         (all-valid-elements? (list-ref a 0))
         (string? (list-ref a 1))
         (string? (list-ref a 2))
         (procedure? (list-ref a 3)))))

;Selectores
(define attack-cost
  (lambda (a)
    (list-ref a 0)))



(define attack-name
  (lambda (a)
    (list-ref a 1)))


(define attack-text
  (lambda (a)
    (list-ref a 2)))


(define attack-function
  (lambda (a)
    (list-ref a 3)))

;Otras funciones

(define attack-has-name?
  (lambda (a nombre)
    (if (string=? (string-downcase (attack-name a)) (string-downcase nombre))
        #t
        #f)))

(define (attack-can-use? a energias-disponibles)
  (let ((costo (attack-cost a))
        (energias (map elem->symbol energias-disponibles)))
    (revisar-todo costo energias)))

(define (revisar-todo costo energias)
  (if (null? costo)
      #t
      (let ((actual (elem->symbol (car costo))))
        (if (eq? actual 'colorless)
            (revisar-todo (cdr costo) energias)
            (if (esta-en-lista? actual energias)
                (revisar-todo (cdr costo) (quitar-primero actual energias))
                #f)))))

(define (esta-en-lista? elemento lista)
  (if (null? lista)
      #f
      (if (eq? elemento (car lista))
          #t
          (esta-en-lista? elemento (cdr lista)))))

(define (quitar-primero elemento lista)
  (if (null? lista)
      '()
      (if (eq? elemento (car lista))
          (cdr lista)
          (cons (car lista) (quitar-primero elemento (cdr lista))))))


(define (attack->string a)
  (string-append
    "  Ataque  : " (attack-name a) "\n"
    "  Costo   : [" (lista-a-string (attack-cost a)) "]\n"
    "  Efecto  : " (attack-text a) "\n"))

(define (lista-a-string lista)
  (if (null? lista)
      "(ninguno)"
      (unir-elementos lista)))

(define (unir-elementos lista)
  (if (null? (cdr lista))
      (symbol->string (car lista))
      (string-append (symbol->string (car lista))
                     ", "
                     (unir-elementos (cdr lista)))))
