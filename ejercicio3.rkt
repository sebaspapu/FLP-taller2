#lang eopl

;;-----------------------------------------
;; Jairo Hernan Gonzalez Barreto - 202324314
;; Sebastian Bolaños Morales 202156743
;;-----------------------------------------
 
#|
 Ejercicio 3 - Evaluacion de Instancias SAT

 Gramática BNF (representación concreta - entrada)
 
 <fnc>       ::= FNC <num> <clausulas>
 
 <clausulas> ::= ( <clausula> )
               | ( <clausula> and <clausulas> )
 
 <clausula>  ::= ( <literales> )
 
 <literales> ::= <literal>
               | <literal> or <literales>
 
 <literal>   ::= <num>
               | - <num>
 
 ──────────────────────────────────────────────
 AST basado en listas (representación abstracta - sobre la que opera EVALUARSAT)
 
 <fnc-ast>        ::= (FNC <num> <clausulas-ast>)
 <clausulas-ast>  ::= (<clausula-ast>)
                    | (<clausula-ast> <clausulas-ast>)
 <clausula-ast>   ::= (<literal-ast> ...)
 <literal-ast>    ::= <num>          ; positivo: variable verdadera
                    | - <num>        ; negativo: variable negada (falsa)
 
 ──────────────────────────────────────────────
 Gramática del resultado
 
 <resultado> ::= (satisfactible <asignacion>)
               | (insatisfactible '())
 
 <asignacion> ::= (#t | #f ...)     ; un booleano por cada variable
 
 
 La idea de EVALUARSAT es recibir una instancia FNC en representacion
 abstracta basada en listas y decir si existe alguna asignacion de
 valores de verdad que la satisfaga o no.

 Asi que para esto generamos todas las combinaciones
 posibles de valores para las n variables (son 2^n combinaciones) y
 probamos cada una hasta encontrar una que satisfaga todas las clausulas,
 o hasta agotar todas sin exito.
|#
 
;; ============================================================
;; FUNCIONES AUXILIARES
;; ============================================================
 
;; eval-literal : Num x ListaDeBools -> Bool
;; Evalúa un literal dado un vector de asignación de variables.
;; Las variables se indexan desde 1.
;; Un literal positivo n  es verdadero si la variable n es #t.
;; Un literal negativo -n es verdadero si la variable n es #f.
(define eval-literal
  (lambda (lit asignacion)
    (if (> lit 0)
        (list-ref asignacion (- lit 1))
        (not (list-ref asignacion (- (- lit) 1))))))
 
 
;; eval-clausula : ListaDeLits x ListaDeBools -> Bool
;; Evalúa una cláusula (disyunción de literales) con una asignación dada.
;; Retorna #t si al menos un literal de la cláusula es verdadero.
(define eval-clausula
  (lambda (clausula asignacion)
    (cond
      [(null? clausula) #f]
      [(eval-literal (car clausula) asignacion) #t]
      [else (eval-clausula (cdr clausula) asignacion)])))
 
 
;; eval-clausulas : ListaDeClausulas x ListaDeBools -> Bool
;; Evalúa una lista de cláusulas (conjunción) con una asignación dada.
;; Retorna #t solo si TODAS las cláusulas son verdaderas.
(define eval-clausulas
  (lambda (clausulas asignacion)
    (cond
      [(null? clausulas) #t]
      [(not (eval-clausula (car clausulas) asignacion)) #f]
      [else (eval-clausulas (cdr clausulas) asignacion)])))
 
 
;; generar-combinaciones : Num -> ListaDeListas
;; Genera todas las combinaciones posibles de valores booleanos
;; para n variables. Retorna una lista de listas de #t/#f.
;; El total de combinaciones es 2^n.
(define generar-combinaciones
  (lambda (n)
    (if (= n 0)
        '(())
        (let ([sub (generar-combinaciones (- n 1))])
          (append
            (map (lambda (c) (cons #t c)) sub)
            (map (lambda (c) (cons #f c)) sub))))))
 
 
;; buscar-solucion : ListaDeClausulas x ListaDeCombinaciones -> Result
;; Recorre todas las combinaciones de asignación hasta encontrar
;; una que satisfaga la FNC, o hasta agotarlas todas.
;; Retorna (satisfactible <asignacion>) o (insatisfactible '()).
(define buscar-solucion
  (lambda (clausulas combinaciones)
    (cond
      [(null? combinaciones) (list 'insatisfactible '())]
      [(eval-clausulas clausulas (car combinaciones))
       (list 'satisfactible (car combinaciones))]
      [else (buscar-solucion clausulas (cdr combinaciones))])))
 
 
;; ============================================================
;; FUNCIÓN PRINCIPAL
;; ============================================================
 
;; EVALUARSAT : FNC-AST -> (satisfactible <asignacion>) | (insatisfactible '())
;; Recibe una instancia FNC en representación AST basada en listas y evalúa
;; si existe alguna asignación de valores de verdad que la satisfaga.
;;
;; Tenemos que si es satisfactible, retorna (satisfactible (#t/#f ...)) con la asignación.
;; Si no lo es, retorna (insatisfactible '()).
(define EVALUARSAT
  (lambda (fnc-ast)
    (let* ([numvars   (cadr fnc-ast)]
           [clausulas (caddr fnc-ast)]
           [combins   (generar-combinaciones numvars)])
      (buscar-solucion clausulas combins))))
 
 
;; ============================================================
;; PRUEBAS
;; ============================================================
 
;; Prueba 1: SATISFACTIBLE
;; C = (x ∨ ¬y ∨ z ∨ w) ∧ (¬y ∨ z) ∧ (¬x ∨ ¬y ∨ ¬z) ∧ (z ∨ w) ∧ y
(EVALUARSAT '(FNC 4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2))))
;; -> (satisfactible (#f #t #t #t))
 
;; Prueba 2: INSATISFACTIBLE
;; C = (x ∨ y) ∧ (¬x) ∧ (¬y)
(EVALUARSAT '(FNC 2 ((1 2) (-1) (-2))))
;; -> (insatisfactible ())
 
;; Prueba 3: SATISFACTIBLE simple
;; C = (x ∨ z) ∧ (¬x ∨ y) ∧ (¬y ∨ ¬z)
(EVALUARSAT '(FNC 3 ((1 3) (-1 2) (-2 -3))))
;; -> (satisfactible (...))
 
;; Prueba 4: Una sola variable, siempre satisfactible
;; C = (x)
(EVALUARSAT '(FNC 1 ((1))))
;; -> (satisfactible (#t))
 
;; Prueba 5: Una sola variable, insatisfactible
;; C = (x) ∧ (¬x)
(EVALUARSAT '(FNC 1 ((1) (-1))))
;; -> (insatisfactible ())