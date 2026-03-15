#lang eopl

;;-----------------------------------------
;; Jairo Hernan Gonzalez Barreto - 202324314
;; Sebastian Bolaños Morales 202156743
;;-----------------------------------------

#|
 Gramática
 
 AST (ahora con su representación abstracta)
 
 <fnc-ast>       ::= (FNC <num> <clausulas-ast>)
 <clausulas-ast> ::= ( <clausula-ast> )
                   | ( <clausula-ast> <clausulas-ast> )
 <clausula-ast>  ::= ( <num> ... )   ; lista plana de literales
|#
 
;; ============================================================
;; PARSE: BNF -> AST
;; ============================================================
 
;; parse-clausula : ClausulaBNF -> ClausulaAST
;; Convierte una cláusula en representación BNF a su AST,
;; eliminando los operadores 'or y dejando solo los literales.
(define parse-clausula
  (lambda (cl)
    (cond
      [(null? cl) '()]
      [(number? (car cl)) (cons (car cl) (parse-clausula (cdr cl)))]
      [(eqv? (car cl) 'or) (parse-clausula (cdr cl))]
      [else '()])))
 
 
;; parse-clausulas : ClausulasBNF -> ClausulasAST
;; Convierte una lista de cláusulas en BNF a su representación AST,
;; eliminando los operadores 'and entre cláusulas.
(define parse-clausulas
  (lambda (cls)
    (cond
      [(null? (cdr cls)) (list (parse-clausula (car cls)))]
      [else
       (cons (parse-clausula (car cls))
             (parse-clausulas (cddr cls)))])))
 
 
;; PARSEBNF : BNF -> AST
;; Convierte una fórmula SAT en representación BNF
;; a su árbol de sintaxis abstracta (AST) basado en listas.
(define PARSEBNF
  (lambda (fnc)
    (list (car fnc)
          (cadr fnc)
          (parse-clausulas (caddr fnc)))))
 
 
;; ============================================================
;; UNPARSE: AST -> BNF
;; ============================================================
 
;; unparse-clausula : ClausulaAST -> ClausulaBNF
;; Convierte una cláusula AST (lista plana de literales)
;; a su representación BNF, insertando 'or entre literales.
(define unparse-clausula
  (lambda (cl)
    (cond
      [(null? (cdr cl)) (list (car cl))]
      [else
       (cons (car cl) (cons 'or (unparse-clausula (cdr cl))))])))
 
 
;; unparse-clausulas : ClausulasAST -> ClausulasBNF
;; Convierte una lista de cláusulas AST a su representación BNF,
;; insertando 'and entre cada cláusula.
(define unparse-clausulas
  (lambda (cls)
    (cond
      [(null? (cdr cls)) (list (unparse-clausula (car cls)))]
      [else
       (cons (unparse-clausula (car cls))
             (cons 'and (unparse-clausulas (cdr cls))))])))
 
 
;; UNPARSEBNF : AST -> BNF
;; Convierte un árbol de sintaxis abstracta (AST) de una instancia SAT
;; a su representación concreta BNF.
;; Es la función inversa de PARSEBNF.
(define UNPARSEBNF
  (lambda (lst)
    (list (car lst)
          (cadr lst)
          (unparse-clausulas (caddr lst)))))
 
 
;; ============================================================
;; PRUEBAS
;; ============================================================
 
;; --- Pruebas PARSEBNF ---
 
;; Prueba 1: instancia del enunciado
(PARSEBNF '(FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and (-1 or -2 or -3) and (3 or 4) and (2))))
;; -> (FNC 4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2)))
 
;; Prueba 2: instancia insatisfactible
(PARSEBNF '(FNC 2 ((1 or 2) and (-1) and (-2))))
;; -> (FNC 2 ((1 2) (-1) (-2)))
 
;; Prueba 3: una sola cláusula
(PARSEBNF '(FNC 1 ((1))))
;; -> (FNC 1 ((1)))
 
 
;; --- Pruebas UNPARSEBNF ---
 
;; Prueba 1: inversa de parse
(UNPARSEBNF '(FNC 4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2))))
;; -> (FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and (-1 or -2 or -3) and (3 or 4) and (2)))
 
;; Prueba 2
(UNPARSEBNF '(FNC 2 ((1 2) (-1) (-2))))
;; -> (FNC 2 ((1 or 2) and (-1) and (-2)))
 
;; Prueba 3: verificar que parse y unparse son inversas
(equal?
  (UNPARSEBNF (PARSEBNF '(FNC 3 ((1 or -2 or 3) and (-1 or 2) and (-3)))))
  '(FNC 3 ((1 or -2 or 3) and (-1 or 2) and (-3))))
;; -> #t