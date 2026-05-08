#lang racket


(provide
  attack
  ability
  attack?
  attack-cost
  attack-name
  attack-text
  attack-function
  attack->string
  attack-has-name?
  attack-can-use?
  elem->symbol)

;TDA ATTACK
;;; REPRESENTACIÓN:


; Se representa internamente como una lista exacta de 4 elementos:
; 1. Costo    (list)      : Lista de símbolos que representan las energías necesarias para atacar (fire water)).
; 2. Nombre   (string)    : El nombre del ataque.
; 3. Texto    (string)    : La descripción del ataque (útil para extraer el daño base).
; 4. Función  (procedure) : El procedimiento lógico que se ejecuta para aplicar los efectos en el juego.

;Tipo de elemento
(define ELEMENT-TYPE
  '(grass fire water lightning psychic fighting darkness metal colorless fairy))

;FUNCIONES DE VALIDACION AUXS


; Descripción: Verifica de forma sencilla si un elemento dado  existe dentro de los tipos permitidos .
; Dom: e (symbol/any)
; Rec: boolean
; Tipo recursión: No aplica
(define valid-element?
  (lambda (e)
    (if (symbol? e)
        (if (esta-en-la-lista? e ELEMENT-TYPE)
            #t
            #f)
        (if (string? e)
            #f
            #f))))
;funcion auxiliar
; Descripción: Función auxiliar que busca paso a paso si un elemento específico existe dentro de una lista de elementos.
; Dom: elemento (any) X lista (list)
; Rec: boolean
; Tipo recursión: Cola
(define esta-en-la-lista?
  (lambda (elemento lista)
    (if (null? lista)
        #f
        (if (eq? elemento (car lista))
            #t
            (esta-en-la-lista? elemento (cdr lista))))))
; Descripción: Revisa una lista entera de energías y confirma que todas sean elementos válidos del juego.
; Dom: lst (list)
; Rec: boolean
; Tipo recursión: Cola
(define all-valid-elements?
  (lambda (lst)
    (cond
      [(null? lst) #t]
      [(not (valid-element? (car lst))) #f]
      [else (all-valid-elements? (cdr lst))])))
;funcion auxiliar
; Descripción: Transforma un texto a un símbolo en minúsculas.
; Dom: e (string/symbol/any)
; Rec: symbol/any
; Tipo recursión: No aplica
(define elem->symbol
  (lambda (e)
    (cond
      [(symbol? e) e]
      [(string? e) (string->symbol (string-downcase e))]
      [else e])))


;CONSTRUCTOR

; Descripción: Es el constructor principal. Crea un ataque empaquetando su costo de energía, su nombre, lo que hace  y su efecto .
; Dom: costo (list) X nombre (string) X texto (string) X funcion (procedure)
; Rec: attack (list)
; Tipo recursión: No aplica
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

;CONSTRUCTOR ability

; Descripción:es un ataque especial que se configura con un costo de 0 energías.
; Dom: nombre (string) X texto (string) X funcion (procedure)
; Rec: attack (list)
; Tipo recursión: No aplica
(define ability
  (lambda (nombre texto funcion)
    (attack '() nombre texto funcion)))


;FUNCION DE PERTENENCIA

; Descripción: Verifica si una lista tiene exactamente 4 elementos y los datos correctos para ser considerada un ataque válido.
; Dom: a (any)
; Rec: boolean
; Tipo recursión: No aplica
(define attack?
  (lambda (a)
    (and (list? a)
         (= (length a) 4)
         (list? (list-ref a 0))
         (all-valid-elements? (list-ref a 0))
         (string? (list-ref a 1))
         (string? (list-ref a 2))
         (procedure? (list-ref a 3)))))


;SELECTORES

; Descripción: Selector que extrae y te entrega la lista de energías que cuesta usar el ataque.
; Dom: a (attack)
; Rec: list
; Tipo recursión: No aplica
(define attack-cost
  (lambda (a)
    (list-ref a 0)))

; Descripción: Selector que extrae y te entrega el nombre del ataque.
; Dom: a (attack)
; Rec: string
; Tipo recursión: No aplica
(define attack-name
  (lambda (a)
    (list-ref a 1)))

; Descripción: Selector que extrae y te entrega la descripción escrita de lo que hace el ataque.
; Dom: a (attack)
; Rec: string
; Tipo recursión: No aplica
(define attack-text
  (lambda (a)
    (list-ref a 2)))

; Descripción: Selector que extrae y te entrega el código  que ejecuta el efecto del ataque en el juego.
; Dom: a (attack)
; Rec: procedure
; Tipo recursión: No aplica
(define attack-function
  (lambda (a)
    (list-ref a 3)))

;OTRAS FUNCIONES

; Descripción: Compara si el nombre del ataque coincide exactamente con un nombre que tú le des, sin importar si usas mayúsculas o minúsculas.
; Dom: a (attack) X nombre (string)
; Rec: boolean
; Tipo recursión: No aplica
(define attack-has-name?
  (lambda (a nombre)
    (string-ci=? (attack-name a) nombre)))
; Descripción: Calcula inteligentemente si las energías que tiene tu Pokémon en juego alcanzan para pagar el costo de este ataque.
; Dom: a (attack) X energias-disponibles (list)
; Rec: boolean
; Tipo recursión: No aplica directamente (se apoya en una función recursiva)
(define (attack-can-use? a energias-disponibles)
  (let ((costo (attack-cost a))
        (energias (map elem->symbol energias-disponibles)))
    (revisar-todo costo energias)))
;Funcion auxiliar
; Descripción: Revisa una por una las energías que requiere el costo. 
; Dom: costo (list) X energias (list)
; Rec: boolean
; Tipo recursión: Cola
(define (revisar-todo costo energias)
  (if (null? costo)
      #t
      (let ((actual (elem->symbol (car costo))))
        (if (eq? actual 'colorless)
            (revisar-todo (cdr costo) energias)
            (if (esta-en-lista? actual energias)
                (revisar-todo (cdr costo) (quitar-primero actual energias))
                #f)))))
;Funcion auxiliar
; Descripción: Busca recursivamente si un elemento puntual está metido dentro de una lista.
; Dom: elemento (any) X lista (list)
; Rec: boolean
; Tipo recursión: Cola
(define (esta-en-lista? elemento lista)
  (if (null? lista)
      #f
      (if (eq? elemento (car lista))
          #t
          (esta-en-lista? elemento (cdr lista)))))
;Funcion auxiliar
; Descripción: Borra únicamente la primera vez que aparece un elemento en una lista..
; Dom: elemento (any) X lista (list)
; Rec: list
; Tipo recursión: Natural
(define (quitar-primero elemento lista)
  (if (null? lista)
      '()
      (if (eq? elemento (car lista))
          (cdr lista)
          (cons (car lista) (quitar-primero elemento (cdr lista))))))
; Descripción: Transforma toda la información técnica del ataque en un texto ordenado .
; Dom: a (attack)
; Rec: string
; Tipo recursión: No aplica
(define (attack->string a)
  (string-append
    "  Ataque  : " (attack-name a) "\n"
    "  Costo   : [" (lista-a-string (attack-cost a)) "]\n"
    "  Efecto  : " (attack-text a) "\n"))
; Descripción: Verifica si la lista de energías está vacía
; Dom: lista (list)
; Rec: string
; Tipo recursión: No aplica
(define (lista-a-string lista)
  (if (null? lista)
      "(ninguno)"
      (unir-elementos lista)))
; Descripción: Toma una lista de elementos y los pega uno al lado del otro separados por comas
; Dom: lista (list)
; Rec: string
; Tipo recursión: Natural
(define (unir-elementos lista)
  (if (null? (cdr lista))
      (symbol->string (car lista))
      (string-append (symbol->string (car lista))
                     ", "
                     (unir-elementos (cdr lista)))))
