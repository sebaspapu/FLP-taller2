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


;; PREDICADOS (estructuras de listas)

;; Verifica si una estructura corresponde a una cláusula válida del AST
;; donde la clausula es una disyuncion de literales (OR).

;; clausula? : Exp → Boolean
(define es-clausula? (lambda (cl)
          (cond
            [(not (list? cl)) #f]
            [(null? cl) #f]
            [(and (null? (cdr cl)) (number? (car cl))) #t]
            [(and (number? (car cl)) (equal? (cadr cl) 'or)) (es-clausula? (cddr cl))]
            [else #f])))


;; Verifica si una estructura corresponde a una lista válida de cláusulas en el AST
;; donde las cláusulas estan conectadas por conjunción (AND).

;; clausulas? : Exp → Boolean
(define es-clausulas? (lambda (cls)
           (cond
             [(null? cls) #f]
             [(and(es-clausula? (car cls)) (null? (cdr cls))) #t]
             [(and (es-clausula? (car cls)) (equal? (cadr cls) 'and)) (es-clausulas? (cddr cls))]
             [else #f])))


;; Predicado que verifica si una expresión tiene la estructura
;; de una fórmula en Forma Normal Conjuntiva (FNC)

;; es-fnc? : Exp -> Boolean
(define es-fnc? (lambda (fnc-exp)
                  (and (list? fnc-exp) (>= (length fnc-exp) 3)
                       (equal? (car fnc-exp) 'FNC) (es-clausulas? (caddr fnc-exp)))
                  ))


;; Implementación con DataTypes (estructuras abstratas)

;; Tipo de dato que representa una clausula de la FNC.
;; Una cláusula es una disyunción (OR) de literales.
(define-datatype clausula clausula?
  (un-literal (lit number?))
  (or-literal (lit number?) (resto clausula?)))

;; Tipo de dato que representa una lista de cláusulas
;; conectadas mediante el operador lógico AND.
(define-datatype clausulas clausulas?
  (una-clausula (cl clausula?))
  (and-clausulas (cl clausula?) (resto clausulas?)))

;; Tipo de dato que representa una fórmula en
;; Forma Normal Conjuntiva (FNC).
(define-datatype fnc fnc?
  (fnc-exp (numvars number?) (cls clausulas?)))


;; PARSE

;; Convierte una cláusula en BNF a su representación AST
;; eliminando los operadores 'or'.
;;
;; Ejemplo:
;; (parse-clausula '(1 or -2 or 3)) -> (1 -2 3)
;;
;; parse-clausula : ClausulaBNF -> ClausulaAST
(define parse-clausula (lambda (cl)
    (cond
      [(null? cl) empty]
      [(number? (car cl)) (cons (car cl) (parse-clausula (cdr cl)))]
      [(eqv? (car cl) 'or) (parse-clausula (cdr cl))]
      [else empty])))


;; Convierte una lista de clausulas en BNF a su representación AST
;; eliminando el operador 'and'.
;;
;; Ejemplo:
;; (parse-clausulas '((1 or 2) and (-3))) -> ((1 2) (-3))
;;
;; parse-clausulas : ClausulasBNF -> ClausulasAST
(define parse-clausulas
  (lambda (cls)
    (cond
      [(null? (cdr cls)) (list (parse-clausula (car cls)))]
      [else
       (cons (parse-clausula (car cls))
             (parse-clausulas (cddr cls)))])))


;; Convierte una formula SAT en BNF a su arbol de sintaxis abstrata (AST)
;;
;; Ejemplo:
;; (FNC 3 '((1 or 2) and (3))) -> (FNC 3 ((1 2) (3)))
;;
;; PARSEBNF : BNF -> AST
(define PARSEBNF (lambda (fnc)
    (list (car fnc) (cadr fnc) (parse-clausulas (caddr fnc)))
 ))


;; UNPARSE


;; Convierte una clausula del AST a su representacinn BNF
;; insertando el operador 'or'.
;;
;; Ejemplo:
;; (unparse-clausula '(-1 9 3)) -> (-1 or 9 or 3)
;;
;; unparse-clausula : ClausulaAST -> ClausulaBNF
(define unparse-clausula (lambda (cl)
    (cond
      [(null? (cdr cl)) (list (car cl))]
      [else
       (cons (car cl) (cons 'or (unparse-clausula (cdr cl))))]
     ))
 )

;; Convierte una lista de clausulas AST en su representación BNF
;; insertando el operador "and" entre cada clausula
;;
;; Ejemplo:
;; (unparse-clausulas '((1 2) (3 -1))) -> ((1 or 2) and (3 or -1))
;;
;; unparse-clausulas : ClausulasAST -> ClausulasBNF
(define unparse-clausulas (lambda (cls)
    (cond
      [(null? (cdr cls)) (list (unparse-clausula (car cls)))]
      [else
       (cons (unparse-clausula (car cls)) (cons 'and (unparse-clausulas (cdr cls))))]
     )))


;; Inversa de PARSEBNF, recibe un arbol de sintaxis abstracta (AST)
;; y lo convierte nuevamente a la gramatica BNF
;;
;; Ejemplo:
;; (UNPARSEBNF '(FNC 4 ((1 -2) (-3 4)))) -> (FNC 4 ((1 or -2) and (-3 or 4)))
;;
;; UNPARSEBNF : AST -> BNF
(define UNPARSEBNF (lambda (lst)
    (list (car lst) (cadr lst) (unparse-clausulas (caddr lst)))))
