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


(define name->str
  (lambda (n)
    (cond
      ((symbol? n) (symbol->string n))
      ((string? n) n)
      (else ""))))

(define count-by-name
  (lambda (nombre lista acc)
    (cond
      ((null? lista) acc)
      ((string-ci=? (name->str (card-name (car lista))) nombre)
       (count-by-name nombre (cdr lista) (+ acc 1)))
      (else
       (count-by-name nombre (cdr lista) acc)))))


(define basic-energy?
  (lambda (c)
    (card-energy? c)))


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


(define has-basic-pokemon?
  (lambda (cartas)
    (cond
      ((null? cartas) #f)
      ((card-basic-pokemon? (car cartas)) #t)
      (else (has-basic-pokemon? (cdr cartas))))))

(define all-cards?
  (lambda (cartas)
    (cond
      ((null? cartas) #t)
      ((not (card? (car cartas))) #f)
      (else (all-cards? (cdr cartas))))))


(define deck
  (lambda cartas
    (let ((lista (if (and (= (length cartas) 1) (list? (car cartas)))
                     (car cartas)
                     cartas)))
      (cond
        ((not (= (length lista) 60))
         (error "deck: el mazo debe tener exactamente 60 cartas"
                (string-append "\n  Cartas recibidas: "
                               (number->string (length lista)))))
        ((not (all-cards? lista))
         (error "deck: todos los elementos deben ser cartas válidas"))
        ((not (has-basic-pokemon? lista))
         (error "deck: el mazo debe incluir al menos 1 Pokémon básico"))
        ((not (valid-copies? lista '()))
         (error "deck: máximo 4 copias de una misma carta (excepto energías básicas)"))
        (else
         (list lista))))))

(define deck?
  (lambda (d)
    (and (list? d)
         (= (length d) 1)
         (list? (car d))
         (all-cards? (car d)))))


(define deck-cards
  (lambda (d)
    (list-ref d 0)))


(define deck-size
  (lambda (d)
    (length (deck-cards d))))


(define deck-empty?
  (lambda (d)
    (null? (deck-cards d))))


(define deck-top
  (lambda (d)
    (if (deck-empty? d)
        #f
        (car (deck-cards d)))))


(define deck-remove-top
  (lambda (d)
    (if (deck-empty? d)
        (error "deck-remove-top: el mazo está vacío")
        (list (cdr (deck-cards d))))))


(define deck-add-top
  (lambda (d c)
    (list (cons c (deck-cards d)))))


(define deck-add-bottom
  (lambda (d c)
    (list (append (deck-cards d) (list c)))))


(define randomPuro
  (lambda (Xn)
    (modulo (+ (* Xn 1103515245) 12345) 2147483648)))


(define list-ref-deck
  (lambda (lst idx)
    (cond
      ((= idx 0) (car lst))
      (else (list-ref-deck (cdr lst) (- idx 1))))))


(define list-set
  (lambda (lst idx nuevo-val)
    (cond
      ((null? lst) '())
      ((= idx 0) (cons nuevo-val (cdr lst)))
      (else (cons (car lst) (list-set (cdr lst) (- idx 1) nuevo-val))))))


(define generar-claves
  (lambda (n semilla)
    (cond
      ((= n 0) '())
      (else
       (let ((nueva (randomPuro semilla)))
         (cons nueva (generar-claves (- n 1) nueva)))))))


(define insertar-ordenado
  (lambda (par lista)
    (cond
      ((null? lista) (list par))
      ((<= (car par) (car (car lista)))
       (cons par lista))
      (else
       (cons (car lista) (insertar-ordenado par (cdr lista)))))))


(define ordenar-pares
  (lambda (lista)
    (cond
      ((null? lista) '())
      (else
       (insertar-ordenado (car lista)
                          (ordenar-pares (cdr lista)))))))

(define shuffleDeck
  (lambda (d semilla)
    (if (not (deck? d))
        (error "shuffleDeck: se requiere un deck válido")
        (let* ((cartas  (deck-cards d))
               (n       (length cartas))
               (claves  (generar-claves n semilla))
               (pares   (map cons claves cartas))
               (ordenados (ordenar-pares pares))
               (resultado (map cdr ordenados)))
          (list resultado)))))