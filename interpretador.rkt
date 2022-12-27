#lang eopl
;;Interpretador
;; URL Github: https://github.com/rwxp/FinalFLP
;;Sebastián Caicedo Martínez - 1841245
;;Laura Moyano Gonzalez - 2023906
;;Santiago Trujillo Ramírez - 2071655
;;Cristian Camilo Montaño Rentería - 2024223
;;Santiago ospitia jimenez - 2025465
;; Proyecto FLP

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      un-programa (exp)>
;;  <expression>    ::= <number>
;;                      <numero-lit (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= var {<identifier> = <expression>}*(,) in <expression> Inspirado por Javascript
;;                      <decVar-exp (ids rands body)>  
;;                  ::= const {<identifier> = <expression>}*(,) in <expression> Inspirado por Javascript
;;                      <const-exp (ids exps body)>
;;                  ::= rec {<identifier> = ({<identifier>}*(,)) = <expression>}* in <expression>
;;                      <rec-exp (rators idss exps body)>
;; ENTREGA ANTERIOR:
;;                  ::= (<expression> <primitiva-binaria> <expression>)
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;                  ::= <expression>
;;                      <primapp-un-exp (prim-unaria exp) >
;;                  ::= Si <expression> entonces <expression> sino <expression> finSI
;;                      <condicional-exp (test-exp true-exp false-exp)>
;;                  ::= declarar ( {<identifier> = <expression>}*(;) ) { <expression> }
;;                      <variableLocal-exp (ids rands body)>
;;                  ::= procedimiento (<identificador>*',') haga <expresion> finProc
;;                      <procedimiento-ex (ids cuerpo)>
;;                  ::= evaluar <expression> ( { <expression>}*) finEval
;;                      <app-exp (exp body)>
;;                  ::= declaraRec ( {<identifier>( {identifier}^*(,) ) = <expression>}^* ) { <expression> }
;;                      <declaraRec-exp (proc-names idss bodies letrec-body)>
;;  <primitiva-binaria>  ::= + | - | * | / | concat
;;  <primitiva-unaria>   ::= add1 | sub1 | longitud

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit))) symbol)
  (text
   ("\"" (or letter whitespace "_")
              (arbno (or letter digit whitespace
                         "." "," ":" ";" "-" "*"
                         "{" "}" "+" "¡" "!" "¿"
                         "?" "=" "'" "@" "#" "%"
                         "$" "&" "/" "(" ")" ">" "<" "|"
                         )) "\"") string)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)
    (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) un-programa)
    
    (expression ("x8" "("(separated-list number ",")")") base8-num-exp)
    (expression ("x16" "("(separated-list number ",")")") base16-num-exp)
    (expression ("x32" "("(separated-list number ",")")") base32-num-exp)

    ;;identificador
    (expression (identifier) var-exp)
    ;;definiciones
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) decVar-exp)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) const-exp)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) rec-exp)
    ;;datos
    (expression (number) numero)
    (expression (text) cadena)
    ;;booleanos
    (expr-bool ("true") true-val)
    (expr-bool ("false") false-val)
    ;;Constructores de Datos Predefinidos
    (expression ("["(separated-list expression ",")"]") lista)
    (expression ("tupla""["(separated-list expression ",")"]") tupla)
    (expression ("{"(separated-list identifier "=" expression ",")"}") registro)
    
    ;;predicados -> bool
    (expr-bool (pred-prim "(" expression "," expression ")") pred-bool)
    ;;operaciones sobre booleanos
;    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-binaria-bool)
;    (expr-bool (oper-un-bool "("expr-bool")") oper-unaria-bool)

    ;;Estructuras de control
    (expression
     ("begin" (separated-list expression ";") "end") begin-exp)
    (expression
     ("if" expr-bool "then" expression "[" "else" expression "]" "end") condicional-exp)
;    (expression
;     ("while" expr-bool  "do" expression "done") while-exp)
    (expression
     ("for" identifier "=" expression "to" expression "do" expression "done") for-exp)
    
    (pred-prim (">") great)
    (pred-prim (">=") great-eq)
    (pred-prim ("<") less)
    (pred-prim ("<=") less-eq)
    (pred-prim ("==") equal)
    (pred-prim ("<>") not-equal)

;    (oper-bin-bool ("and") and-op)
;    (oper-bin-bool ("or") or-op)
;
;    (oper-un-bool ("not") not-op)

    
    ;;GRAMATICAS TALLER PASADO:
    (expression
     ("("  expression primitiva-binaria expression ")") primapp-bin-exp)
    (expression
     ( primitiva-unaria "(" expression ")") primapp-un-exp)
    (expression
     ("declarar" "("(separated-list identifier "=" expression ";")")" "{" expression "}") variableLocal-exp)
    (expression
     ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expression "finProc") procedimiento-ex)
    (expression ( "evaluar" expression "("(separated-list expression ",") ")"  "finEval" ) app-exp)
    (expression ("declaraRec" "(" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) ")" "{" expression "}") 
                declaraRec-exp)

    ;;sobre enteros
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("%") primitiva-div)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)

    ;;sobre cadenas
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-binaria ("concat") primitiva-concat)
    ))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)l
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (body)
                 (eval-expression body (init-env))))))
;init-env: ()--> <extend-env>
; función que retorna un ambiente inicial en forma de sintaxis abstracta.
; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e @pi) 
     '(1 2 3 "hola" "FLP" 3.141592) 
     (empty-env))))

; eval-expression: <expression> <enviroment> -> numero
; evalua la expresión para cada caso de la gramatica y recibe un ambiente
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (base8-num-exp (nums) nums)
      (base16-num-exp (nums) nums)
      (base32-num-exp (nums) nums)
      (numero (datum) datum)
      (cadena (txt) (normalizar txt))
      (var-exp (id) (buscar-variable env id))
      (decVar-exp (ids rands body) ids)
      (const-exp (ids rands body) ids)
      (rec-exp (proc-names idss bodies rec-body) proc-names)
      (lista (values) values)
      (tupla(values) values)
      (registro(ids exps)ids)
      (begin-exp (exps) exps)
      (condicional-exp (test-exp true-exp false-exp) (if (eval-bool test-exp env) (eval-expression true-exp env)
                                                         (eval-expression false-exp env)))
;      (while-exp(test-exp true-exp) test-exp)
      (for-exp (id init-value final-value body) id)
;      (pred-bool (pred exp1 exp2) pred)
;      (oper-binaria-bool (op exp1 exp2) op)
;      (oper-unaria-bool (op exp1) exp1)
      
      ;;ENTREGA ANTERIOR:
      (primapp-bin-exp (rand1 prim-bin rand2)
                   (let ((args  (eval-expression rand1 env))
                         (args2 (eval-expression rand2 env)))
                     (apply-primitiva-binaria prim-bin args args2)))
      (primapp-un-exp (prim-un rand)
                   (let ((args (eval-expression rand env)))
                     (apply-primitiva-unaria prim-un args)))
      (variableLocal-exp (ids rands body)
              (let ((args (eval-rands rands env)))
                (eval-expression body
                                 (extend-env ids args env))))
      (procedimiento-ex (ids cuerpo)
                (cerradura ids cuerpo env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (declaraRec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env))))))

(define eval-bool
  (lambda(bool-exp env)
    (cases expr-bool bool-exp
      (true-val () "true")
      (false-val () "false")
      (pred-bool (pred exp1 exp2) ((eval-pred pred) (eval-expression exp1 env) (eval-expression exp2 env)))
    )))

(define eval-pred
  (lambda(pred)
    (cases pred-prim pred
      (great () >)
      (great-eq () >=)
      (less () <)
      (less-eq () <=)
      (equal () =)
      (not-equal () (lambda(exp1 exp2)(not (= exp1 exp2)))))
    ))

; eval-rands: funciones auxiliares para aplicar eval-rand a cada elemento de una 
; lista de operandos (expresiones)
; <lista> <enviroment> -> <lista>
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

; eval-rand: funcion auxiliar para aplicar eval-expression a un elemento de una 
; lista de operandos (expresiones)
; <structure-rand> <enviroment> -> <numero>
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitiva-binaria: <primitiva> <expression> <expression> -> numero | text
;proposito: aplica una función primitiva binaria a dos argumentos recibidos arg1 arg2
(define apply-primitiva-binaria
  (lambda (prim arg1 arg2 )
    (cases primitiva-binaria prim
      (primitiva-suma () (+ arg1 arg2))
      (primitiva-resta () (- arg1 arg2))
      (primitiva-multi () (* arg1 arg2))
      (primitiva-div () (/ arg1 arg2))
      (primitiva-concat() (string-append arg1 arg2)) 
      )))


;apply-primitiva-unaria: <primitiva> <expression> -> numero
;proposito: aplica una función primitiva unaria a un argumento recibido arg
(define apply-primitiva-unaria
  (lambda(prim arg)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length (normalizar arg)))
      (primitiva-add1 () (+ arg 1))
      (primitiva-sub1 () (- arg 1))
      
    )))

;normalizar: función que elimina los backslash "\" de una string
(define normalizar
  (lambda (s)
    (if (string-ci=? s "") ""
        (if (string-ci=? (substring s 0 1) "\"")
            (normalizar(substring s 1))
            (string-append(substring s 0 1) (normalizar(substring s 1)))))
    )
  )

;Define el tipo de dato Procedimiento, el cual es una cerradura que se compone de:
; una lista de identificadores, seguido de una expresión y por último un ambiente.
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expression?)
   (amb environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

;Definicion de los tipos de dato booleano.
(define scheme-value? (lambda (v) #t))

;empty-env:    -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> extended-enviroment
;función que extiende un ambiente.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'buscar-variable "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env sym)))))))


;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

;list-find-position: <symbol> <list-of-symbol> -> number
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;list-index: <procedure> <list-of-symbol> -> number | boolean
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


(show-the-datatypes)
just-scan
scan&parse

;******************************************************************************************
;Parte 2

; Punto a
; Programa que calcula el area de un circulo dado el radio del circulo.
;
;declarar(
;  @radio=2.5;
;  @areaCirculo=
;       procedimiento(@radio)
;            haga (@pi * (@radio * @radio))
;       finProc
;){
;
;  evaluar @areaCirculo(@radio) finEval
;}
;ejemplos
;@radio=2 = 12.566368;
;@radio=4 = 50.265472;

;Punto b
;Este programa se encarga de calcular el factorial de un número mediante la recursividad. La idea de esta función
;es multiplicar todos los números que hay entre el número 1, y el número que se pase como parámetro.
;declaraRec(
;           @factorial(@n) = Si @n entonces (@n*evaluar @factorial(sub1(@n)) finEval) sino 1 finSI
;           ){
;             evaluar @factorial(5) finEval
;             }
;ejemplos
;evaluar @factorial(5) finEval = 120
;evaluar @factorial (10) finEval = 3628800


;Punto c
;Este programa recursivo utiliza la recursividad para sumar dos números. En este caso, en cada llamado recursivo se le va
;restando una unidad al número @a, y a su vez sumando una unidad al número @b. Una vez el número @a llegue a 0, la función
;retorna el valor almacenado en el número @b luego de todas las iteraciones.
;declaraRec(
;           @sumar(@a, @b) = Si @a entonces evaluar @sumar(sub1(@a), add1(@b)) finEval sino @b finSI
;           ){
;             evaluar @sumar(4, 5) finEval
;             }
;ejemplos
;evaluar @sumar(4, 5) finEval = 9
;evaluar @sumar(8, 8) finEval = 16

;Punto d
;Este programa permite restar y multiplicar dos números haciendo
; uso solamente de las primitivas add1 y sub1.
; Para la resta funciona decrementando el valor de las variables @a y @b.
; Para la multiplicación funciona sumando el valor de @a las veces de @b hasta  que llegue a 0.
;
; declaraRec (
;   @sumar(@a, @b) = Si @a entonces
;     evaluar @sumar(sub1(@a), add1(@b)) finEval
;     sino @b finSI
;   @multiplicar (@a , @b) = Si @b entonces
;     evaluar @sumar(@a, evaluar @multiplicar(@a,sub1(@b)) finEval) finEval
;     sino 0 finSI
;   @restar(@a , @b) = Si @b entonces
;     evaluar @restar(sub1(@a),sub1(@b)) finEval
;     sino @a finSI)
;   {
;    evaluar @multiplicar (10, 3) finEval  
;   }
;ejemplos
;evaluar @multiplicar(4, 5) finEval = 20
;evaluar @multiplicar (10, 3) finEval = 30
;evaluar @restar (10, 3) finEval =7
;evaluar @restar(8, 8) finEval = 0


;Punto e
;Es un programa que se encarga de crear en primer lugar una función @integrantes, la cual retorna el nombre
; de los integrantes del grupo en forma de texto. Después se crea la función @saludar
; que recibe @integrantes y retorna una función que concatena el string "Hola" junto
; a la evaluación de @integrantes. Finalmente declara una variable @decorate que
; almacenará justamente a la función retornada por la evaluación de @saludar con @integrantes.
;
;declarar(
;     @integrantes = procedimiento() haga "Laura Cristian Santiago O Santiago T y Sebastian" finProc
;){ 
;   declarar(
;     @saludar = procedimiento(@integrantes) haga
;        procedimiento() haga ("Hola " concat evaluar @integrantes() finEval) finProc
;        finProc
;   ){
;      declarar(
;        @decorate = evaluar @saludar(@integrantes) finEval 
;      ){
;        evaluar @decorate() finEval
;      } 
;   }
;}




;Punto f
;Es un programa que se encarga de crear en primer lugar una función @integrantes, la cual retorna el nombre
; de los integrantes del grupo en forma de texto. Después se crea la función @saludar la cual recibe @integrantes y retorna
; una función que recibe un parametro @mensaje, concatena el string "Hola" junto a la evaluación de @integrantes y al final concatena
; el parametro @mensaje ingresado por el usuario.
; Luego se declara la variable @decorate, quien evalua a @saludar con @integrantes, y en el cuerpo de la estructura declarar
; se evalúa @decorate con el parámetro " ProfesoresFLP".
;
;declarar(
;     @integrantes = procedimiento() haga "Laura Cristian Santiago O Santiago T y Sebastian" finProc
;){ 
;   declarar(
;     @saludar = procedimiento(@integrantes) haga
;        procedimiento(@mensaje) haga (("Hola " concat evaluar @integrantes() finEval) concat @mensaje) finProc
;        finProc
;   ){
;      declarar(
;        @decorate = evaluar @saludar(@integrantes) finEval 
;      ){
;        evaluar @decorate("_ProfesorFLP") finEval
;      } 
;   }
;}
;Ejemplos/Pruebas
;evaluar @decorate(" Nos vemos en la próxima clase") finEval
;evaluar @decorate("_Aprobaron la materia :D")



