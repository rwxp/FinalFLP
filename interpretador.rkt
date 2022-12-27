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
    
    ;;números hexadecimales
    (expression ("x8" "("(separated-list number ",")")") base8-num-exp)
    (expression ("x16" "("(separated-list number ",")")") base16-num-exp)
    (expression ("x32" "("(separated-list number ",")")") base32-num-exp)

    ;;identificador
    (expression (identifier) identificador)
    ;;definiciones
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) decVar-exp)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) const-exp)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)
                       "in" expression) rec-exp)
    ;;datos
    (expression (number) numero)
    (expression (text) cadena)
    
    ;;Constructores de Datos Predefinidos
    (expression ("["(separated-list expression ",")"]") lista)
    (expression ("tupla""["(separated-list expression ",")"]") tupla)
    (expression ("{"(separated-list identifier "=" expression ",")"}") registro)

    ;;Estructuras de control
    (expression
     ("begin" (separated-list expression ";") "end") begin-exp)
    (expression
     ("if" expr-bool "then" expression "[" "else" expression "]" "end") condicional-exp)
    (expression
     ("while" expr-bool  "do" expression "done") while-exp)
    (expression
     ("for" identifier "=" expression "to" expression "do" expression "done") for-exp)


    ;;expr-bool
    (bool ("true") true-val)
    (bool ("false") false-val)    
    (expr-bool (pred-prim "(" expression "," expression ")") pred-bool)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-binaria-bool)
    (expr-bool (oper-un-bool "("expr-bool")") oper-unaria-bool)
    
    (pred-prim (">") great)
    (pred-prim (">=") great-eq)
    (pred-prim ("<") less)
    (pred-prim ("<=") less-eq)
    (pred-prim ("==") equal)
    (pred-prim ("<>") not-equal)

    (oper-bin-bool ("and") and-op)
    (oper-bin-bool ("or") or-op)
    (oper-un-bool ("not") not-op)

    
    ;;operaciones binarias y unarias 
    (expression
     ("("  expression primitiva-binaria expression ")") primapp-bin-exp)
    (expression
     ( primitiva-unaria "(" expression ")") primapp-un-exp)

    ;;procedimiento
    (expression
     ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expression "finProc") procedimiento-ex)
    ;;recursiva
    (expression ( "evaluar" expression "("(separated-list expression ",") ")"  "finEval" ) app-exp)
    

    ;;sobre enteros
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("%") primitiva-mod)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)

    ;;Sobre base 8, 16 y 32
    ;;aritmeticas base 8
    (primitiva-binaria ("octal+") primitiva-suma-octal)
    (primitiva-binaria ("octal~") primitiva-resta-octal)
    (primitiva-binaria ("octal*") primitiva-multiplicacion-octal)
    (primitiva-unaria ("octal-add1") primitiva-add1-octal)
    (primitiva-unaria ("octal-sub1") primitiva-sub1-octal)
    ;;aritméticas base 16
    (primitiva-binaria ("hexa+") primitiva-suma-hexa)
    (primitiva-binaria ("hexa~") primitiva-resta-hexa)
    (primitiva-binaria ("hexa*") primitiva-multiplicacion-hexa)
    (primitiva-unaria ("hexa-add1") primitiva-add1-hexa)
    (primitiva-unaria ("hexa-sub1") primitiva-sub1-hexa)
    ;;aritméticas base 32
    (primitiva-binaria ("32+") primitiva-suma-32)
    (primitiva-binaria ("32~") primitiva-resta-32)
    (primitiva-binaria ("32*") primitiva-multiplicacion-32)
    (primitiva-unaria ("32-add1") primitiva-add1-32)
    (primitiva-unaria ("32-sub1") primitiva-sub1-32)

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
      
      ;base 8, 16 y 32
      (base8-num-exp (nums) nums)
      (base16-num-exp (nums) nums)
      (base32-num-exp (nums) nums)

      ;;datos
      (numero (datum) datum)
      (cadena (txt) (normalizar txt))
      (identificador (id) (buscar-variable env id))

      ;;definiciones
      (decVar-exp (ids rands body)
              (let ((args (eval-rands rands env)))
                (eval-expression body
                                 (extend-env ids args env))))
      (const-exp (ids rands body) ids)
      
      (rec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))

      ;;datos predefinidos
      (lista (values) values)
      (tupla(values) values)
      (registro(ids exps)ids)

      ;;estructura de control
      (begin-exp (exps) exps)
      (condicional-exp (test-exp true-exp false-exp) (if (eval-bool test-exp env) (eval-expression true-exp env)
                                                         (eval-expression false-exp env)))
      (while-exp(test-exp true-exp) test-exp)
      (for-exp (id init-value final-value body) id)
     
      
      ;;aplicar primitivas unaria y binaria
      (primapp-bin-exp (rand1 prim-bin rand2)
                   (let ((args  (eval-expression rand1 env))
                         (args2 (eval-expression rand2 env)))
                     (apply-primitiva-binaria prim-bin args args2)))
      (primapp-un-exp (prim-un rand)
                   (let ((args (eval-expression rand env)))
                     (apply-primitiva-unaria prim-un args)))

      ;;procedimiento
      (procedimiento-ex (ids cuerpo)
                (cerradura ids cuerpo env))
      
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
     )))


(define (eval-bool bool-val env)
  (cases bool bool-val
    (true-val  () "true")
    (false-val () "false")
    
 ))
(define eval-bool-exp
  (lambda(bool-exp env)
    (cases expr-bool bool-exp    
      (pred-bool (pred exp1 exp2) ((eval-pred pred) (eval-expression exp1 env) (eval-expression exp2 env)))
      (oper-binaria-bool (op exp1 exp2) op)
      (oper-unaria-bool (op exp1) exp1)
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
      ;;aritmeticas base 8
      (primitiva-suma-octal() (sumaOctal arg1 arg2 ))
      (primitiva-resta-octal () (restaOctal arg1 arg2))
      (primitiva-multiplicacion-octal () (multiOctal arg1 arg2))
      ;;aritmeticas base 16
      (primitiva-suma-hexa() (sumaHexa arg1 arg2))
      (primitiva-resta-hexa () (restaHexa arg1 arg2))
      (primitiva-multiplicacion-hexa () (multiHexa arg1 arg2))
      ;;aritmeticas base 32
      (primitiva-suma-32() (suma32 arg1 arg2))
      (primitiva-resta-32 () (resta32 arg1 arg2))
      (primitiva-multiplicacion-32 () (multi32 arg1 arg2))
      ;;aritmeticas enteros
      (primitiva-suma () (+ arg1 arg2))
      (primitiva-resta () (- arg1 arg2))
      (primitiva-multi () (* arg1 arg2))
      (primitiva-div () (/ arg1 arg2))
      (primitiva-mod () (modulo arg1 arg2))
      ;;aritmetica cadena
      (primitiva-concat() (string-append arg1 arg2)) 
      )))

;---------Funciones que operan con los numeros en las bases 8, 16, 32 -----------
(define is-zero?
  (lambda (n)
    (or (null? n) (equal?  n '(0)))
    )
  )
;;Funcion que retorna el siguiente valor de un octal
;;octal -> octal
(define successorOctal
  (lambda (l)
    (cond
      [(is-zero? l)'(1)]
      [(< (car l) 7) (cons (+ 1 (car l)) (cdr l))]
      [(eqv? 7 (car l)) (cons 0 (successorOctal (cdr l)))]
      )
    )
  )
;;Funcion que retorna el valor previo de un octal
;;octal -> octal
(define predecessorOctal
  (lambda (l)
    (cond
      [(and (is-zero? (list (car l)))(not (null?(cdr l)))) (cons 7 (predecessorOctal (cdr l)))]
      [(is-zero? l) (0)]
      [(and (eqv? (car l) 1) (null? (cdr l))) empty]
      [(> 8 (car l)) (cons (- (car l) 1) (cdr l))]
      )
    )
  )
;;Funcion que suma dos Octales y retorna el resultado en Octales
;;l1,l2 : Octal -> Octal
(define sumaOctal
  (lambda (l1 l2)
    (cond
      [(is-zero? l2)l1]
      [(is-zero? l1)l2]
      [else (sumaOctal (successorOctal l1)(predecessorOctal l2))]
      )
    )
  )
;;Funcion que resta dos Octales y retorna el resultado en Octal
;;l1,l2 : Octal -> Octal
(define restaOctal
  (lambda (l1 l2)
    (cond
      [(and (is-zero? l1)(not(is-zero? l2)))(eopl:error "No se cubren los resultados negativos")]
      [(is-zero? l2)l1]
      [(is-zero? l1)(0)]
      [else (restaOctal (predecessorOctal l1)(predecessorOctal l2))]
      )
    )
  )
;;Funcion que multiplica dos Octales y retorna el resultado en Octal
;;op1,op2 : Octal -> Octal
(define multiOctal
  (lambda (op1 op2)
    (cond
      [(or(is-zero? op1)(is-zero? op2))(0)]
      [(is-zero? (predecessorOctal op2))op1]
      [else (sumaOctal op1 (multiOctal op1 (predecessorOctal op2)))]
      )
    )
  )

;;Funcion que retorna el siguiente valor de un Hexadecimal
;;bigNum16 -> bigNum16
(define successorHexa
  (lambda (l)
    (cond
      [(is-zero? l)'(1)]
      [(< (car l) 15) (cons (+ 1 (car l)) (cdr l))]
      [(eqv? 15 (car l)) (cons 0 (successorHexa (cdr l)))]
      )
    )
  )
;;Funcion que retorna el valor previo de un Hexadecimal
;;bigNum16 -> bigNum16
(define predecessorHexa
  (lambda (l)
    (cond
      [(and (is-zero? (list (car l)))(not (null?(cdr l)))) (cons 15 (predecessorHexa (cdr l)))]
      [(is-zero? l) (0)]
      [(and (eqv? (car l) 1) (null? (cdr l))) empty]
      [(> 16 (car l)) (cons (- (car l) 1) (cdr l))]
      )
    )
  )

;;Funcion que suma dos bigNum16 y retorna el resultado en bigNum16
;;l1,l2 : bigNum16 -> bigNum16
(define sumaHexa
  (lambda (l1 l2)
    (cond
      [(is-zero? l2)l1]
      [(is-zero? l1)l2]
      [else (sumaHexa (successorHexa l1)(predecessorHexa l2))]
      )
    )
  )
;;Funcion que resta dos bigNum16 y retorna el resultado en bigNum16
;;l1,l2 : bigNum16 -> bigNum16
(define restaHexa
  (lambda (l1 l2)
    (cond
      [(and (is-zero? l1)(not(is-zero? l2)))(eopl:error "No se cubren los resultados negativos")]
      [(is-zero? l2)l1]
      [(is-zero? l1)(0)]
      [else (restaHexa (predecessorHexa l1)(successorHexa l2))]
      )
    )
  )
;;Funcion que multiplica dos bigNum16 y retorna el resultado en bigNum16
;;op1,op2 : bigNum16 -> bigNum16
(define multiHexa
  (lambda (op1 op2)
    (cond
      [(or(is-zero? op1)(is-zero? op2))(0)]
      [(is-zero? (predecessorHexa op2))op1]
      [else (sumaHexa op1 (multiHexa op1 (predecessorHexa op2)))]
      )
    )
  )

;;Funcion que retorna el siguiente valor de un base 32
;;bigNum32 -> bigNum32
(define successor32
  (lambda (l)
    (cond
      [(is-zero? l)'(1)]
      [(< (car l) 31) (cons (+ 1 (car l)) (cdr l))]
      [(eqv? 31 (car l)) (cons 0 (successor32 (cdr l)))]
      )
    )
  )
;;Funcion que retorna el valor previo de un bigNum32
;;bigNum32 -> bigNum32
(define predecessor32
  (lambda (l)
    (cond
      [(and (is-zero? (list (car l)))(not (null?(cdr l)))) (cons 31 (predecessor32 (cdr l)))]
      [(is-zero? l) (0)]
      [(and (eqv? (car l) 1) (null? (cdr l))) empty]
      [(> 32 (car l)) (cons (- (car l) 1) (cdr l))]
      )
    )
  )

;;Funcion que suma dos bigNum32 y retorna el resultado en bigNum32
;;l1,l2 : bigNum32 -> bigNum32
(define suma32
  (lambda (l1 l2)
    (cond
      [(is-zero? l2)l1]
      [(is-zero? l1)l2]
      [else (suma32 (successor32 l1)(predecessor32 l2))]
      )
    )
  )
;;Funcion que resta dos bigNum32 y retorna el resultado en bigNum32
;;l1,l2 : bigNum32 -> bigNum32
(define resta32
  (lambda (l1 l2)
    (cond
      [(and (is-zero? l1)(not(is-zero? l2)))(eopl:error "No se cubren los resultados negativos")]
      [(is-zero? l2)l1]
      [(is-zero? l1)(0)]
      [else (resta32 (predecessor32 l1)(successor32 l2))]
      )
    )
  )
;;Funcion que multiplica dos bigNum32 y retorna el resultado en bigNum32
;;op1,op2 : bigNum32 -> bigNum32
(define multi32
  (lambda (op1 op2)
    (cond
      [(or(is-zero? op1)(is-zero? op2))(0)]
      [(is-zero? (predecessor32 op2))op1]
      [else (suma32 op1 (multi32 op1 (predecessor32 op2)))]
      )
    )
  )



;apply-primitiva-unaria: <primitiva> <expression> -> numero
;proposito: aplica una función primitiva unaria a un argumento recibido arg
(define apply-primitiva-unaria
  (lambda(prim arg)
    (cases primitiva-unaria prim
      ;;sobre cadena
      (primitiva-longitud () (string-length (normalizar arg)))
      ;;sobre entero
      (primitiva-add1 () (+ arg 1))
      (primitiva-sub1 () (- arg 1))
      ;;sobre base diferente
      (primitiva-add1-octal() (successorOctal arg))
      (primitiva-sub1-octal () (predecessorOctal arg))
      (primitiva-add1-hexa() (successorHexa arg))
      (primitiva-sub1-hexa () (predecessorHexa arg))
      (primitiva-add1-32() (successor32 arg))
      (primitiva-sub1-32 () (predecessor32 arg))
      
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
