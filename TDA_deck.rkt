#lang racket


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