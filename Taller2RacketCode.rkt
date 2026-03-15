#lang eopl
#|
Jairo Hernan Gonzalez Barreto - 202324314
Sebastian saramanbiche 
|#

#|
 Gramática BNF

 <fnc> ::= FNC <num> <clausulas>

 <clausulas> ::= ( <clausula> )
               | ( <clausula> and <clausulas> )

 <clausula> ::= ( <literales> )

 <literales> ::= <literal>
               | <literal> or <literales>

 <literal> ::= <num>
               | - <num>
|#


;; Constructores

(define constructor-clausula (lambda (vars)
          (cond
            [(null? (cdr vars))(list (car vars))]
            [else (cons (car vars) (cons 'or(constructor-clausula (cdr vars))))]
            )))

(define constructor-clausulas (lambda (cls)
          (cond
            [(null? (cdr cls)) (list (car cls))]
            [else (cons (car cls) (cons 'and (constructor-clausulas (cdr cls))))])))

(define constructor-FNC (lambda (numvars cls) (list 'FNC numvars cls)))


;; Extractores

(define fnc->numvars (lambda (fnc-exp) (car (cdr fnc-exp))))

(define fnc->clausulas (lambda (fnc-exp) (car (cddr fnc-exp))))

(define clausulas->primera-clausula (lambda (lista-clausulas) (car
lista-clausulas)))

(define clausulas->resto-clausulas (lambda (lista-clausulas) (cddr
lista-clausulas)))

(define clausula->varbools (lambda (cl)
          (cond
            [(null? cl) empty]
            [(equal? (car cl) 'or) (clausula->varbools (cdr cl))]
            [else (cons (car cl) (clausula->varbools (cdr cl)))])))


;; Predicados - estructuras de listas

(define es-clausula? (lambda (cl)
          (cond
            [(not (list? cl)) #f]
            [(null? cl) #f]
            [(and (null? (cdr cl)) (number? (car cl))) #t]
            [(and (number? (car cl)) (equal? (cadr cl) 'or)) (es-clausula? (cddr cl))]
            [else #f])))

(define es-clausulas? (lambda (cls)
           (cond
             [(null? cls) #f]
             [(and(es-clausula? (car cls)) (null? (cdr cls))) #t]
             [(and (es-clausula? (car cls)) (equal? (cadr cls) 'and)) (es-clausulas? (cddr cls))]
             [else #f])))

(define es-fnc? (lambda (fnc-exp)
                  (and (list? fnc-exp) (>= (length fnc-exp) 3)
                       (equal? (car fnc-exp) 'FNC) (es-clausulas? (caddr fnc-exp)))
                  ))


;; DataTypes - estructuras abstratas

(define-datatype clausula clausula?
  (un-literal (lit number?))
  (or-literal (lit number?) (resto clausula?)))

(define-datatype clausulas clausulas?
  (una-clausula (cl clausula?))
  (and-clausulas (cl clausula?) (resto clausulas?)))

(define-datatype fnc fnc?
  (fnc-exp (numvars number?) (cls clausulas?)))



;; PARSE

;; Convierte una clausula BNF a lista de literales
;; (1 or 2 or -3) -> (1 2 -3)

(define parse-clausula (lambda (cl)
    (cond
      [(null? cl) empty]
      [(number? (car cl)) (cons (car cl) (parse-clausula (cdr cl)))]
      [(equal? (car cl) 'or) (parse-clausula (cdr cl))]
      [else empty])))


;; Convierte clausulas BNF
;; ((1 or 2) and (3 or -1)) -> ((1 2) (3 -1))

(define parse-clausulas(lambda (cls)
    (cond
      [(null? (cdr cls)) (list (parse-clausula (car cls)))]
      [else
       (cons (parse-clausula (car cls)) (parse-clausulas (cddr cls)))])))


;; Convierte una FNC completa
;; PARSEBNF ::= BNF -> AST

(define PARSEBNF (lambda (fnc)
    (list (car fnc) (cadr fnc) (parse-clausulas (caddr fnc)))))




;; UNPARSE

;; Convierte (1 2 -3) -> (1 or 2 or -3)

(define unparse-clausula (lambda (cl)
    (cond
      [(null? (cdr cl)) (list (car cl))]
      [else
       (cons (car cl) (cons 'or (unparse-clausula (cdr cl))))])))


;; Convierte ((1 2) (3 -1)) -> ((1 or 2) and (3 or -1))

(define unparse-clausulas (lambda (cls)
    (cond
      [(null? (cdr cls)) (list (unparse-clausula (car cls)))]
      [else
       (cons (unparse-clausula (car cls)) (cons 'and (unparse-clausulas (cdr cls))))]
     )))


;; Convierte AST basado en listas a BNF
;; UNPARSEBNF ::= AST -> BNF
(define UNPARSEBNF (lambda (lst)
    (list (car lst) (cadr lst) (unparse-clausulas (caddr lst)))))