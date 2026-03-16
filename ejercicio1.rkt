#lang eopl

;;-----------------------------------------
;; Jairo Hernan Gonzalez Barreto - 202324314
;; Sebastian Bolaños Morales 202156743
;;-----------------------------------------
 
#|

 Ejercicio 1 - Gramatica BNF y representaciones de instancias SAT
 
 Gramática BNF
 
 <fnc>      ::= FNC <num> <clausulas>
 
 <clausulas> ::= ( <clausula> )
               | ( <clausula> and <clausulas> )
 
 <clausula>  ::= ( <literales> )
 
 <literales> ::= <literal>
               | <literal> or <literales>
 
 <literal>   ::= <num>
               | - <num>

 Implementamos dos versiones: una basada en listas y otra en datatypes.
 Para la de listas definimos constructores, extractores y predicados.
 Para la de datatypes usamos define-datatype directamente.

|#
 
;; ============================================================
;; Implementación basada en LISTAS
;; ============================================================
 
;; ------------------ Constructores ------------------
 
;; constructor-clausula : ListaDeNums -> ClausulaBNF
;; Construye una cláusula BNF (disyunción) a partir de una lista de literales.
;; Inserta el operador 'or entre cada literal.
(define constructor-clausula
  (lambda (vars)
    (cond
      [(null? (cdr vars)) (list (car vars))]
      [else (cons (car vars) (cons 'or (constructor-clausula (cdr vars))))])))
 
 
;; constructor-clausulas : ListaDeClausulas -> ClausulasBNF
;; Construye una lista de cláusulas BNF (conjunción) a partir de una lista de cláusulas.
;; Inserta el operador 'and entre cada cláusula.
(define constructor-clausulas
  (lambda (cls)
    (cond
      [(null? (cdr cls)) (list (car cls))]
      [else (cons (car cls) (cons 'and (constructor-clausulas (cdr cls))))])))
 
 
;; constructor-FNC : Num x ClausulasBNF -> FNC
;; Construye una expresión FNC completa dado el número de variables y las cláusulas.
(define constructor-FNC
  (lambda (numvars cls)
    (list 'FNC numvars cls)))
 
 
;; ------------------ Extractores ------------------
 
;; fnc->numvars : FNC -> Num
;; Extrae el número de variables de una expresión FNC.
(define fnc->numvars
  (lambda (fnc-exp)
    (car (cdr fnc-exp))))
 
 
;; fnc->clausulas : FNC -> ClausulasBNF
;; Extrae la lista de cláusulas de una expresión FNC.
(define fnc->clausulas
  (lambda (fnc-exp)
    (car (cddr fnc-exp))))
 
 
;; clausulas->primera-clausula : ClausulasBNF -> ClausulaBNF
;; Extrae la primera cláusula de una lista de cláusulas.
(define clausulas->primera-clausula
  (lambda (lista-clausulas)
    (car lista-clausulas)))
 
 
;; clausulas->resto-clausulas : ClausulasBNF -> ClausulasBNF
;; Extrae el resto de las cláusulas (saltando el 'and).
(define clausulas->resto-clausulas
  (lambda (lista-clausulas)
    (cddr lista-clausulas)))
 
 
;; clausula->varbools : ClausulaBNF -> ListaDeNums
;; Extrae los literales de una cláusula BNF, eliminando los 'or.
(define clausula->varbools
  (lambda (cl)
    (cond
      [(null? cl) '()]
      [(equal? (car cl) 'or) (clausula->varbools (cdr cl))]
      [else (cons (car cl) (clausula->varbools (cdr cl)))])))
 
 
;; ------------------ Predicados ------------------
 
;; es-clausula? : Exp -> Boolean
;; Verifica si una estructura corresponde a una cláusula BNF válida
;; (disyunción de literales numéricos separados por 'or).
(define es-clausula?
  (lambda (cl)
    (cond
      [(not (list? cl)) #f]
      [(null? cl) #f]
      [(and (null? (cdr cl)) (number? (car cl))) #t]
      [(and (number? (car cl)) (equal? (cadr cl) 'or)) (es-clausula? (cddr cl))]
      [else #f])))
 
 
;; es-clausulas? : Exp -> Boolean
;; Verifica si una estructura corresponde a una lista válida de cláusulas BNF
;; (cláusulas conectadas por 'and).
(define es-clausulas?
  (lambda (cls)
    (cond
      [(null? cls) #f]
      [(and (es-clausula? (car cls)) (null? (cdr cls))) #t]
      [(and (es-clausula? (car cls)) (equal? (cadr cls) 'and)) (es-clausulas? (cddr cls))]
      [else #f])))
 
 
;; es-fnc? : Exp -> Boolean
;; Verifica si una expresión tiene la estructura de una FNC válida.
(define es-fnc?
  (lambda (fnc-exp)
    (and (list? fnc-exp)
         (>= (length fnc-exp) 3)
         (equal? (car fnc-exp) 'FNC)
         (es-clausulas? (caddr fnc-exp)))))
 
 
;; ============================================================
;; EJEMPLOS - (listas)
;; ============================================================
 
;; Instancia 1: C = (x ∨ ¬y ∨ z ∨ w) ∧ (¬y ∨ z) ∧ (¬x ∨ ¬y ∨ ¬z) ∧ (z ∨ w) ∧ y
;; Variables: x=1, y=2, z=3, w=4
(define clausula1-1 (constructor-clausula '(1 -2 3 4)))
(define clausula1-2 (constructor-clausula '(-2 3)))
(define clausula1-3 (constructor-clausula '(-1 -2 -3)))
(define clausula1-4 (constructor-clausula '(3 4)))
(define clausula1-5 (constructor-clausula '(2)))
(define clausulas1  (constructor-clausulas (list clausula1-1 clausula1-2 clausula1-3 clausula1-4 clausula1-5)))
(define instancia1  (constructor-FNC 4 clausulas1))
;; instancia1 => (FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and (-1 or -2 or -3) and (3 or 4) and (2)))
 
;; Instancia 2: C = (x ∨ y) ∧ (¬x) ∧ (¬y)   -- INSATISFACTIBLE
(define clausula2-1 (constructor-clausula '(1 2)))
(define clausula2-2 (constructor-clausula '(-1)))
(define clausula2-3 (constructor-clausula '(-2)))
(define clausulas2  (constructor-clausulas (list clausula2-1 clausula2-2 clausula2-3)))
(define instancia2  (constructor-FNC 2 clausulas2))
;; instancia2 => (FNC 2 ((1 or 2) and (-1) and (-2)))
 
;; Instancia 3: C = (x ∨ z) ∧ (¬x ∨ y) ∧ (¬y ∨ ¬z)
(define clausula3-1 (constructor-clausula '(1 3)))
(define clausula3-2 (constructor-clausula '(-1 2)))
(define clausula3-3 (constructor-clausula '(-2 -3)))
(define clausulas3  (constructor-clausulas (list clausula3-1 clausula3-2 clausula3-3)))
(define instancia3  (constructor-FNC 3 clausulas3))
;; instancia3 => (FNC 3 ((1 or 3) and (-1 or 2) and (-2 or -3)))
 
;; Pruebas de extractores
(fnc->numvars instancia1)          ;; -> 4
(fnc->clausulas instancia1)        ;; -> ((1 or -2 or 3 or 4) and ...)
(clausulas->primera-clausula clausulas1) ;; -> (1 or -2 or 3 or 4)
(clausulas->resto-clausulas clausulas1)  ;; -> ((-2 or 3) and ...)
(clausula->varbools clausula1-1)         ;; -> (1 -2 3 4)
 
;; Pruebas de predicados
(es-clausula? clausula1-1)  ;; -> #t
(es-clausula? '(1 and 2))   ;; -> #f
(es-clausulas? clausulas1)  ;; -> #t
(es-fnc? instancia1)        ;; -> #t
(es-fnc? '(SAT 2 ((1))))    ;; -> #f
 
 
;; ============================================================
;; Implementación basada en DATATYPES
;; ============================================================
 
;; Tipo de dato que representa una cláusula (disyunción de literales).
;; un-literal  : representa una cláusula de un solo literal.
;; or-literal  : representa la disyunción de un literal con el resto de la cláusula.
(define-datatype clausula clausula?
  (un-literal  (lit number?))
  (or-literal  (lit number?) (resto clausula?)))
 
 
;; Tipo de dato que representa una lista de cláusulas (conjunción).
;; una-clausula  : una única cláusula.
;; and-clausulas : una cláusula seguida del resto de cláusulas.
(define-datatype clausulas clausulas?
  (una-clausula  (cl clausula?))
  (and-clausulas (cl clausula?) (resto clausulas?)))
 
 
;; Tipo de dato que representa una fórmula FNC completa.
;; fnc-exp : contiene el número de variables y la lista de cláusulas.
(define-datatype fnc fnc?
  (fnc-exp (numvars number?) (cls clausulas?)))
 
 
;; ============================================================
;; EJEMPLOS(datatypes)
;; ============================================================
 
;; Instancia DT1: C = (x ∨ ¬y ∨ z ∨ w) ∧ (¬y ∨ z) ∧ (¬x ∨ ¬y ∨ ¬z) ∧ (z ∨ w) ∧ y
(define dt-inst1
  (fnc-exp 4
    (and-clausulas (or-literal 1 (or-literal -2 (or-literal 3 (un-literal 4))))
    (and-clausulas (or-literal -2 (un-literal 3))
    (and-clausulas (or-literal -1 (or-literal -2 (un-literal -3)))
    (and-clausulas (or-literal 3 (un-literal 4))
    (una-clausula  (un-literal 2))))))))
 
;; Instancia DT2: C = (x ∨ y) ∧ (¬x) ∧ (¬y)   -- INSATISFACTIBLE
(define dt-inst2
  (fnc-exp 2
    (and-clausulas (or-literal 1 (un-literal 2))
    (and-clausulas (un-literal -1)
    (una-clausula  (un-literal -2))))))
 
;; Instancia DT3: C = (x ∨ z) ∧ (¬x ∨ y) ∧ (¬y ∨ ¬z)
(define dt-inst3
  (fnc-exp 3
    (and-clausulas (or-literal 1 (un-literal 3))
    (and-clausulas (or-literal -1 (un-literal 2))
    (una-clausula  (or-literal -2 (un-literal -3)))))))
 
;; Verificación de que los datatypes fueron construidos correctamente
(clausula? (un-literal 1))                    ;; -> #t
(clausulas? (una-clausula (un-literal 1)))    ;; -> #t
(fnc? dt-inst1)                               ;; -> #t
(fnc? dt-inst2)                               ;; -> #t
(fnc? dt-inst3)                               ;; -> #t