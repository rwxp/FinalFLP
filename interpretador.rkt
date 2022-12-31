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
;;  <expression>    ::= <numero>
;;                      <numero (datum)>
;;                  ::= <identificador>
;;                      <identificador (id)>
;;                  ::= var {<identificador> = <expression>}*(,) in <expression> Inspirado por Javascript
;;                      <decVar-exp (ids rands body)>  
;;                  ::= const {<identificador> = <expression>}*(,) in <expression> Inspirado por Javascript
;;                      <const-exp (ids exps body)>
;;                  ::= rec {<identificador> = ({<identificador>}*(,)) = <expression>}* in <expression>
;;                      <rec-exp (rators idss exps body)>
;;                  ::= begin {<expression>}+(;) end   Inspirado por Racket.
;;                      <begin-exp (exp exps)>
;;                  ::= set <identificador> = <expression> Inspirado por Racket.
;;                      <set-exp (id rhs)>
;;                  :: <registro> ::= "{" {<identificador>=<expresion>}+(;) "}"
;;                      <registro (ids exps)>
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
   ("#" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit))) symbol)
  (text
   ("\"" (or letter whitespace "_")
              (arbno (or letter digit whitespace
                         "." "," ":" ";" "-" "*"
                         "{" "}" "+" "¡" "!" "¿"
                         "?" "=" "'" "@" "#"
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
    (expression ("crear-tupla" "(" "tupla" "["(separated-list expression ",")"]" ")") tupla-exp)
    (expression ("tupla?" "("  expression ")" ) tuplas?)
    (expression ("vacio?" "(" expression ")") tupla-vacia?)
    (expression ("crear-registro" "(" "{"(separated-list identifier "=" expression ";")"}" ")") crear-registro)
    (expression ("registros?" "(" expression ")") registros?)
    (expression("ref-registro" "(" identifier "," expression ")") ref-registro)
    (expression("set-registro" "(" identifier "," expression "," expression")") set-registro)
    
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                set-exp)
     ;;Estructuras de control
    (expression
     ("if" expression "then" expression "[" "else" expression "]" "end") condicional-exp)
    (expression
     ("while" expression  "do" expression "done") while-exp)
    (expression
     ("for" identifier "=" expression "to" expression "do" expression "done") for-exp)


    ;;expr-bool
    (expression ("true") true-val)
    (expression ("false") false-val)    
    (expression (pred-prim "(" expression "," expression ")") pred-bool)
    (expression (oper-bin-bool "(" expression "," expression ")") oper-binaria-bool)
    (expression (oper-un-bool "("expression")") oper-unaria-bool)
    
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
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))
;**************************************************************************************
;Definición tipos de datos referencia y target para manejar paso por valor y por ref.
(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

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
              (let ((args (eval-var-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      
      (const-exp (ids rands body)
                  (let ((args (eval-var-rands rands env))
                       (idss (map-const-ids ids env)))
                   (if (element-in-list? idss #t)
                        (eopl:error 'const-exp "one or more constanst were already declared before")
                        (eval-expression body (extend-env ids args env))
                        )))
      
      (rec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))

      ;;datos predefinidos
      (lista (values) values)
      (tupla-exp(values) (una-tupla values))
      (tuplas?(exp) (tupla? (eval-expression exp env)))
      (tupla-vacia?(exp) (empty-tupla? (eval-expression exp env)))
      (crear-registro(ids exps) (un-registro ids (eval-rands exps env)))
      (registros?(exp) (registro? (eval-expression exp env)))
      (ref-registro(key registro) (get-value key (eval-expression registro env)))
      (set-registro(key value registro) (set-value key (eval-expression value env) (eval-expression registro env)))
      ;;Secuenciacion:
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (begin-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps) 
                       acc
                       (loop (eval-expression (car exps) 
                                              env)
                             (cdr exps)))))

      ;;estructura de control
      (condicional-exp (test-exp true-exp false-exp) (if (eval-expression test-exp env) (eval-expression true-exp env)
                                                         (eval-expression false-exp env)))
      (true-val  () #t)
      (false-val () #f)
      (pred-bool (pred exp1 exp2) ((eval-pred pred) (eval-expression exp1 env) (eval-expression exp2 env)))
      (oper-binaria-bool (op exp1 exp2) (apply-bool-binaria op (eval-expression exp1 env) (eval-expression exp2 env)))
      (oper-unaria-bool (op exp1) (apply-bool-unaria op (eval-expression exp1 env)))
      (while-exp (test-exp true-exp)
                (let loop ()
                  (if (eval-expression test-exp env)
                        (begin (eval-expression true-exp env)
                        (loop)) 'break)))
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
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
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
(define apply-bool-binaria
  (lambda(op arg1 arg2)
    (cases oper-bin-bool op
      (and-op () (and arg1 arg2))
      (or-op () (or arg1 arg2))
     )
    )
  )

(define apply-bool-unaria
  (lambda(op arg)
    (cases oper-un-bool op
      (not-op () (not arg))
     )
    )
  )

(define empty-tupla? (lambda(tpl) (cases tupla tpl (tupla-vacia () #t) (else #f) )))

;map-const-ids: función auxiliar que hace un mapeo de una lista de elementos verificando si se encuentra uno en un ambiente.
;<listae> <environment> -> <lista>
(define map-const-ids
  (lambda (elem env)
    (map (lambda (x) (element-in-env? x env)) elem)))


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

;eval-var-rand: funcion auxiliar para aplicar eval-expression a un elemento direct-target
;de una lista de operandos(expresiones)
;<struc-rand> <environment> -> <numero>
(define eval-var-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;eval-var-rands: funcion auxiliar para aplicar eval-var-rand a cada elemento de una lista
;de operandos(expresiones)
;<lista> <environment> -> <lista>
(define eval-var-rands
  (lambda (rands env)
    (map (lambda (x) (eval-var-rand x env)) rands)))


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
;-----------------------Estructuras de datos (registro) y funciones-------------
(define-datatype registro registro?
  (vacio)
  (un-registro (ids (list-of symbol?)) (exps (list-of expval?))))

(define-datatype tupla tupla?
  (tupla-vacia)
  (una-tupla (exps (list-of expression?))))

(define get-value
  (lambda(id reg)
    (cases registro reg
      (vacio() (eopl:error "Está intentando obtener un elemento de un registro vacío"))
      (un-registro(ids exps) (search-value id ids exps))
      )
    )
  )

(define search-value
  (lambda(id ids exps)
    (cond
      [(eqv? ids '()) (eopl:error "La clave que ingresó no tiene un valor asociado")]
      [(eqv? id (car ids)) (car exps)]
      [else (search-value id (cdr ids) (cdr exps))]
      ))
  )

(define set-value
  (lambda(id value reg)
    (cases registro reg
      (vacio() (eopl:error "El registro está vacío, no tiene elementos para modificar"))
      (un-registro(ids exps) (un-registro ids (set-index (list->vector exps)(get-index 0 id ids) value)))
      )
    )
  )

(define set-index
  (lambda(vec indx value)
    (begin
      (vector-set! vec indx value)
      (set! vec (vector->list vec))
      vec
      )
    ))

(define get-index
  (lambda(index id ids)
    (cond
      [(eqv? ids '()) #f]
      [(eqv? id (car ids)) index]
      [else (get-index (+ index 1) id (cdr ids))])
    ))

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
      [(and (is-zero? l1) (not(is-zero? l2))) (eopl:error "El programa no es capaz de procesar resultados negativos")]
      [(is-zero? l2)l1]
      [else (restaHexa (predecessorHexa l1)(predecessorHexa l2))]
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
      [(and (is-zero? l1) (not(is-zero? l2))) (eopl:error "El programa no es capaz de procesar resultados negativos")]
      [(is-zero? l2)l1]
      [else (resta32 (predecessor32 l1)(predecessor32 l2))]
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
(define-datatype procval procval?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expression?)
   (amb environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
;(define-datatype environment environment?
;  (empty-env-record)
;  (extended-env-record (syms (list-of symbol?))
;                       (vals (list-of scheme-value?))
;                       (env environment?))
;  (recursively-extended-env-record (proc-names (list-of symbol?))
;                                   (idss (list-of (list-of symbol?)))
;                                   (bodies (list-of expression?))
;                                   (env environment?)))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

;empty-env:    -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (cerradura ids body env))))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente (SIN SECUENCIACION NI PASO POR VALOR).
;(define buscar-variable
;  (lambda (env sym)
;    (cases environment env
;      (empty-env-record ()
;                        (eopl:error 'buscar-variable "No binding for ~s" sym))
;      (extended-env-record (syms vals env)
;                           (let ((pos (list-find-position sym syms)))
;                             (if (number? pos)
;                                 (list-ref vals pos)
;                                 (buscar-variable env sym))))
;      (recursively-extended-env-record (proc-names idss bodies old-env)
;                                       (let ((pos (list-find-position sym proc-names)))
;                                         (if (number? pos)
;                                             (cerradura (list-ref idss pos)
;                                                      (list-ref bodies pos)
;                                                      env)
;                                             (buscar-variable old-env sym)))))))

(define buscar-variable
  (lambda (env sym)
    (deref (apply-env-ref env sym))
    ))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


(define element-in-env?
  (lambda (elem env)
    (cases environment env
      (empty-env-record ()
                        #f)
      (extended-env-record (syms vals env)
                           (let ((ids syms))
                               (if (element-in-list? ids elem)
                                   #t
                                   (element-in-env? elem env)
                                )))
                        )))
;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (registro? x) (boolean? x) (string? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))
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

;element-in-list?: función auxiliar que determina si un elemento se encuentra en una lista.
;<list> <scheme-value> -> bool
(define element-in-list?
  (lambda (lst elem)
    (cond
      ((null? lst) #f)
      ((equal? (car lst) elem) #t)
      (else (element-in-list? (cdr lst) elem)))
       ))

(show-the-datatypes)
just-scan
scan&parse

;;Ejemplos de scan&parse
;numero
(scan&parse "3")
;identificador
(scan&parse "x")
;begin-exp
(scan&parse "begin (3 +2); 2 end")
;set-exp
(scan&parse "set x=3")
; registro
(scan&parse "crear-registro({x=4;y=3})")
;condicional-exp
(scan&parse "if and(true, false) then 1 [else 0] end")
;(scan&parse "begin
;     set x = crear-registro({y=3});
;    set y = ref-registro(w,  crear-registro({w=18}));
;    y
;end")
; 
;while-exp
(scan&parse "begin while <(x,10) do set x=(x+1) done; x end")
;decVar-exp
(scan&parse "var n = 5 in n")
;******************************************************************************************
;Parte 2
