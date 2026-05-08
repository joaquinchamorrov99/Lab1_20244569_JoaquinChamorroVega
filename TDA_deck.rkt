#lang racket



(require "TDA_card.rkt")

(provide
  deck
  deck?
  deck-cards
  deck-size
  deck-empty?
  deck-top
  deck-remove-top
  deck-add-top
  deck-add-bottom
  shuffleDeck)

;TDA DECK

;; REPRESENTACIÓN:
;; Un mazo se representa como una lista de 1 elemento:
;; (list cartas)
;;
;;   cartas : List card -> lista ordenada de 60 cartas
;;
;; REGLAS DEL MAZO:
;;   - Exactamente 60 cartas.
;;   - Máximo 4 copias de una misma carta (por nombre), excepto energías básicas.
;;   - Las energías básicas no tienen límite de copia.
;;   - Al menos 1 Pokémon básico.
;;
;; FUNCIONES AUXILIARES DE VALIDACIÓN

; Descripción: Convierte de forma  el nombre de una carta (sea símbolo o string) a formato string.
; Dom: n (symbol/string)
; Rec: string
; Tipo recursión: No aplica

(define name->str
  (lambda (n)
    (cond
      ((symbol? n) (symbol->string n))
      ((string? n) n)
      (else ""))))
; Descripción: Cuenta cuántas veces aparece una carta con un nombre específico dentro de una lista de cartas.
; Dom: nombre (string) X lista (list) X acc (integer)
; Rec: integer
; Tipo recursión: Cola

(define count-by-name
  (lambda (nombre lista acc)
    (cond
      ((null? lista) acc)
      ((string-ci=? (name->str (card-name (car lista))) nombre)
       (count-by-name nombre (cdr lista) (+ acc 1)))
      (else
       (count-by-name nombre (cdr lista) acc)))))

; Descripción: Verifica si una carta entregada es una carta de energía.
; Dom: card
; Rec: booleano
; Tipo recursión: No aplica

(define basic-energy?
  (lambda (c)
    (card-energy? c)))

; Descripción: Valida que en una lista de cartas no existan más de 4 copias de la misma carta .
; Dom: cartas (list) X revisadas (list)
; Rec: boolean
; Tipo recursión: Cola
(define valid-copies?
  (lambda (cartas revisadas)
    (cond
      ((null? cartas) #t)
      ((basic-energy? (car cartas))
       (valid-copies? (cdr cartas) revisadas))
      ((member (string-downcase (name->str (card-name (car cartas)))) revisadas)
       (valid-copies? (cdr cartas) revisadas))
      (else
       (let ((nombre (string-downcase (name->str (card-name (car cartas)))))
             (cantidad (count-by-name (name->str (card-name (car cartas)))
                                      cartas 0)))
         (if (> cantidad 4)
             #f
             (valid-copies? (cdr cartas)
                            (cons nombre revisadas))))))))

; Descripción: Verifica que una lista de cartas contenga por lo menos un Pokémon basico.
; Dom: cartas (list)
; Rec: boolean
; Tipo recursión: Cola
(define has-basic-pokemon?
  (lambda (cartas)
    (cond
      ((null? cartas) #f)
      ((card-basic-pokemon? (car cartas)) #t)
      (else (has-basic-pokemon? (cdr cartas))))))


; Descripción: Verifica que todos los elementos dentro de una lista sean del TDA card.
; Dom: cartas (list)
; Rec: boolean
; Tipo recursión: Cola
(define all-cards?
  (lambda (cartas)
    (cond
      ((null? cartas) #t)
      ((not (card? (car cartas))) #f)
      (else (all-cards? (cdr cartas))))))

;; CONSTRUCTOR

; Descripción: Crea un mazo validando que tenga exactamente 60 cartas, al menos un Pokémon basico, máximo 4 copias por carta y que  sus elementos sean validos.
; Dom: cartas (list / varargs)
; Rec: deck (list)
; Tipo recursión: No aplica
(define deck
  (lambda cartas
    (let ((lista (if (and (= (length cartas) 1) (list? (car cartas)))
                     (car cartas)
                     cartas)))
      (cond
        ((not (= (length lista) 60))
         (error "deck: el mazo debe tener 60 cartas"
                (string-append "\n  Cartas recibidas: "
                               (number->string (length lista)))))
        ((not (all-cards? lista))
         (error "deck: todos los elementos deben ser cartas"))
        ((not (has-basic-pokemon? lista))
         (error "deck: el mazo debe incluir al menos 1 Pokémon basico"))
        ((not (valid-copies? lista '()))
         (error "deck: máximo 4 copias de una misma carta )"))
        (else
         (list lista))))))

;; FUNCIÓN DE PERTENENCIA

; Descripción: Función de pertenencia que verifica si una estructura cumple con el formato de un mazo.
; Dom: d (any)
; Rec: boolean
; Tipo recursión: No aplica
(define deck?
  (lambda (d)
    (and (list? d)
         (= (length d) 1)
         (list? (car d))
         (all-cards? (car d)))))

;; SELECTORES

; Descripción: Selector que obtiene la lista interna de cartas contenidas en el mazo.
; Dom: d (deck)
; Rec: list
; Tipo recursión: No aplica
(define deck-cards
  (lambda (d)
    (list-ref d 0)))

; Descripción: Selector  que cuenta la cantidad total de cartas que quedan en el mazo.
; Dom: deck
; Rec: integer
; Tipo recursión: No aplica
(define deck-size
  (lambda (d)
    (length (deck-cards d))))

; Descripción: Verifica si el mazo se ha quedado vacio.
; Dom: d (deck)
; Rec: boolean
; Tipo recursión: No aplica
(define deck-empty?
  (lambda (d)
    (null? (deck-cards d))))

; Descripción: Selector que obtiene la carta que se encuentra en la parte superior del mazo ).
; Dom: d (deck)
; Rec: card / booleano
; Tipo recursión: No aplica
(define deck-top
  (lambda (d)
    (if (deck-empty? d)
        #f
        (car (deck-cards d)))))

;; MODIFICADORES

; Descripción: Modificador que extrae y elimina la carta superior del mazo.
; Dom: deck
; Rec: deck (list)
; Tipo recursión: No aplica
(define deck-remove-top
  (lambda (d)
    (if (deck-empty? d)
        (error "deck-remove-top: el mazo está sin cartas")
        (list (cdr (deck-cards d))))))

; Descripción: Modificador que añade una nueva carta arriba del mazo.
; Dom: d (deck) X c (card)
; Rec: deck (list)
; Tipo recursión: No aplica
(define deck-add-top
  (lambda (d c)
    (list (cons c (deck-cards d)))))


(define deck-add-bottom
  (lambda (d c)
    (list (append (deck-cards d) (list c)))))

;; FUNCIÓN DE BARAJADO

; Descripción: Generador de números pseudoaleatorios .
; Dom: Xn (integer)
; Rec: integer
; Tipo recursión: No aplica
(define randomPuro
  (lambda (Xn)
    (modulo (+ (* Xn 1103515245) 12345) 2147483648)))

; Descripción: Obtiene el elemento ubicado en un indice específico dentro de una lista.
; Dom: lst (list) X idx (integer)
; Rec: any
; Tipo recursión: Cola
(define list-ref-deck
  (lambda (lst idx)
    (cond
      ((= idx 0) (car lst))
      (else (list-ref-deck (cdr lst) (- idx 1))))))

; Descripción: Actualiza el valor de un elemento en un índice específico de una lista, retornando la nueva lista.
; Dom: lst (list) X idx (integer) X nuevo-valor 
; Rec: list
; Tipo recursión: Natural
(define list-set
  (lambda (lst idx nuevo-val)
    (cond
      ((null? lst) '())
      ((= idx 0) (cons nuevo-val (cdr lst)))
      (else (cons (car lst) (list-set (cdr lst) (- idx 1) nuevo-val))))))

; Descripción: Genera una lista de 'n' números pseudoaleatorios consecutivos basados en una semilla inicial.
; Dom: n (integer) X semilla (integer)
; Rec: list
; Tipo recursión: Natural
(define generar-claves
  (lambda (n semilla)
    (cond
      ((= n 0) '())
      (else
       (let ((nueva (randomPuro semilla)))
         (cons nueva (generar-claves (- n 1) nueva)))))))

; Descripción: Inserta un par (clave . valor) en una lista de pares de forma ascendente según su clave.
; Dom: par (pair) X lista (list)
; Rec: list
; Tipo recursión: Natural
(define insertar-ordenado
  (lambda (par lista)
    (cond
      ((null? lista) (list par))
      ((<= (car par) (car (car lista)))
       (cons par lista))
      (else
       (cons (car lista) (insertar-ordenado par (cdr lista)))))))

; Descripción: Ordena una lista de pares (clave . valor) apoyándose en la función de inserción ordenada.
; Dom: lista (list)
; Rec: lista
; Tipo recursión: Natural
(define ordenar-pares
  (lambda (lista)
    (cond
      ((null? lista) '())
      (else
       (insertar-ordenado (car lista)
                          (ordenar-pares (cdr lista)))))))
; Descripción: Modificador que baraja aleatoriamente las cartas de un mazo con una semilla dada.
; Dom: d (deck) X semilla (integer)
; Rec: deck (list)
; Tipo recursión: No aplica 
(define shuffleDeck
  (lambda (d semilla)
    (if (not (deck? d))
        (error "shuffleDeck: deck no valido")
        (let* ((cartas  (deck-cards d))
               (n       (length cartas))
               (claves  (generar-claves n semilla))
               (pares   (map cons claves cartas))
               (ordenados (ordenar-pares pares))
               (resultado (map cdr ordenados)))
          (list resultado)))))