#lang racket



(require "TDA_attack.rkt")
(require "TDA_card.rkt")
(require "TDA_deck.rkt")
(require "TDA_player.rkt")
(require "TDA_game.rkt")

(provide

  (all-from-out "TDA_attack.rkt")
  (all-from-out "TDA_card.rkt")
  (all-from-out "TDA_deck.rkt")
  (all-from-out "TDA_player.rkt")
  (all-from-out "TDA_game.rkt")

 
 )


(define nombre->str
  (lambda (n)
    (cond
      ((symbol? n) (symbol->string n))
      ((string? n) n)
      (else ""))))


(define nombre=?
  (lambda (a b)
    (string-ci=? (nombre->str a) (nombre->str b))))


(define (card-in-hand? p c)
  (if (null? c)
      #f
      (buscar-carta? (player-hand p) c)))


(define (buscar-carta? mano c)
  (if (null? mano)
      #f
      (if (son-la-misma-carta? (car mano) c)
          #t
          (buscar-carta? (cdr mano) c))))


(define (son-la-misma-carta? carta1 carta2)
  (if (eq? (card-type carta1) (card-type carta2))
      (if (nombre=? (card-name carta1) (card-name carta2))
          #t
          #f)
      #f))


(define find-pokemon-in-play
  (lambda (p c)
    (cond
      [(null? c) #f]
      [(not (card-pokemon? c)) #f]
      [else
       (let ([name (card-name c)])
         (cond
           
           [(and (not (null? (player-active p)))
                 (nombre=? (card-name (ipc-card (player-active p))) name))
            (list 'active (player-active p))]
           
           [else
            (let loop ([bench (player-bench p)])
              (cond
                [(null? bench) #f]
                [(nombre=? (card-name (ipc-card (car bench))) name)
                 (list 'bench (car bench))]
                [else (loop (cdr bench))]))]))])))


(define find-in-bench
  (lambda (p c)
    (cond
      [(null? c) #f]
      [else
       (let ([name (card-name c)])
         (let loop ([bench (player-bench p)])
           (cond
             [(null? bench) #f]
             [(nombre=? (card-name (ipc-card (car bench))) name)
              (car bench)]
             [else (loop (cdr bench))])))])))


(define replace-in-bench
  (lambda (p old-ipc new-ipc)
    (let ((old-id (ipc-id old-ipc)))
      (define (reemplazar bench)
        (cond
          ((null? bench) '())
          ((= (ipc-id (car bench)) old-id)
           (cons new-ipc (cdr bench)))
          (else
           (cons (car bench) (reemplazar (cdr bench))))))
      (player-set-bench p (reemplazar (player-bench p))))))

(define gen-unique-id
  (lambda (g)
    (abs (game-seed g))))


(define evolve-ipc
  (lambda (ipc new-card)
    (list new-card
          (ipc-energies ipc)
          (ipc-damage ipc)
          "normal"
          0
          (ipc-id ipc))))