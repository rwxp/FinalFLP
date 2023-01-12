#lang eopl
;;Interpretador
;;URL Github: https://github.com/rwxp/FinalFLP
;;Sebastián Caicedo Martínez - 1841245
;;Laura Moyano Gonzalez - 2023906
;;Santiago Trujillo Ramírez - 2071655
;;Cristian Camilo Montaño Rentería - 2024223
;;Santiago ospitia jimenez - 2025465
;;Proyecto FLP

;; La definición BNF para las expresiones del lenguaje inspirado en python:
;;
;;  <program>       ::= (<class-decl>*) <expression>
;;                      <un-programa (class-decl exp)>

;;  <expression>    ::= <numero>
;;                      <numero (datum)>

;;                  ::= x8 (<numero>*)
;;                      < base8-num-exp (datum)>

;;                  ::= x16 (<numero>*)
;;                      < base16-num-exp (datum)>

;;                  ::= x32 (<numero>*)
;;                      < base32-num-exp (datum)>

;;                  := <text> 
;;                     <cadena (txt)>

;;                  ::= <identificador>
;;                      <identificador (id)>

;;                  ::= (<expression> <primitiva-binaria> <expression>)
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>

;;                  ::= <primitiva-unaria>( <expression>)
;;                      <primapp-un-exp (prim-unaria exp)>

;;                  ::= procedimiento (<identificador>*',') haga <expresion> finProc
;;                      <procedimiento-ex (ids cuerpo)>

;;                  ::= evaluar <expression> ( { <expression>}*) finEval
;;                      <app-exp (exp body)>

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

;;                  ::=  tupla[{<expresion>} ∗(;)]
;;                       <tupla-exp (value values)>

;;                  ::=  [{<expresion>} ∗(;)]
;;                       <lista-exp (values)>

;;                  :: <registro> ::= "{" {<identificador>=<expresion>}+(;) "}"
;;                      <registro (ids exps)>

;;                  ::= for <identificador> = <expresion>
;;                        to <expresion> do
;;                        <expresion > done
;;                      <for-exp (ids exp exp exp)>

;;                  ::= if <expr-bool > then <expresion >
;;                      [ else <expression> ] end
;;                      <condicional-exp (exp exp exp)>

;;                  ::= while <expr−bool> do
;;                      <expresion> done
;;                      <while-exp (exp exps)>

;;                 ::= "false"
;;                     <false-val>

;;                 ::= "true"
;;                     <true-val>

;;                 ::= <pred-prim> ( <expresion> , <expresion> )
;;                       <pred-bool (exp1 exp2)>

;;                 ::= <oper-bin-bool> ( <expresion-bool> , <expresion-bool> )
;;                      <oper-binaria-bool (exp1 exp2)>

;;                 ::= <oper-un-bool> (<expresion-bool>)
;;                      <oper-unaria-bool (exp)>

;;                 ::= <print (<expression>)>
;;                 ::= <print-exp (exp)>

;;                 ::= new <identificador> (<expression>*(,))
;;                     new-object-exp (class-name rands)

;;                 ::= send <expression> <identificador> (<expression>*(,))
;;                     method-app-exp (obj-exp method-name rands)

;;                 ::= super <identificador> (<expression>*(,))
;;                     super-call-exp (method-name rands)

;;                 ::= <print-obj (<expression>)>
;;                     <print-obj-exp (exp)>

;;                 ::= <print-obj2 ( <expression> . <identificador> )>
;;                 ::= <print2-obj-exp (exp id)>

;;  <class-decl>   ::= class <identificador> extends <identificador>
;;                     (field<identificador>)* (<method-decl>)*
;;                     a-class-decl(class-name super-name fields-ids method-decls)

;;  <method-decl>  ::= def <identificador> (<identificador>*(,)) <expression>
;;                     a-method-decl (method-name ids body)

;;
;; <pred-prim>     ::= < (less)}
;;                 ::= > (great)
;;                 ::= <= (less-eq)
;;                 ::= >= (great-eq)
;;                 ::= == (equal)
;;                 ::= <> (not-equal)
;;
;; <oper-bin-bool>  ::= and (and-op)
;;                  ::= or (or-op)
;;
;; <oper-un-bool>   ::= not (not-op)
;;
;;  <primitiva-binaria>  ::= +
;;                          | -
;;                          | *
;;                          | /
;;                          | concat
;;                          | octal+ | octal~ | octal*|
;;                          | hexa+ | hexa~ | hexa*|
;;                          | 32+ | 32~ | 32*|

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
  '((program ((arbno class-decl) expression) un-programa)
    
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
    (expression ("crear-lista" "(""["(separated-list expression ";")"]"")") crear-lista)
    (expression ("lista?" "("  expression ")" ) listas?)
    (expression ("vacio-lista" "(" ")") vacia-lista-exp)
    (expression ("vacio-lista?" "(" expression ")") lista-vacia?)
    (expression ("cabeza-lista" "(" expression ")") cabeza-lista-exp)
    (expression ("cola-lista" "(" expression ")") cola-lista-exp)
    (expression("ref-lista" "(" number "," expression ")") ref-lista)
    (expression("set-lista" "(" number "," expression "," expression")") set-lista)
    (expression("append" "(" expression "," expression")") append-lista)
    
    (expression ("crear-tupla" "(" "tupla" "[" expression (arbno ";" expression) "]" ")") tupla-exp)
    (expression ("tupla?" "("  expression ")" ) tuplas?)
    (expression ("ref-tuple" "(" number "," expression ")") ref-tupla-exp)    
    (expression ("vacio-tupla" "(" ")") vacia-tupla-exp)
    (expression ("vacio-tupla?" "(" expression ")") tupla-vacia?)
    (expression ("cabeza-tupla" "(" expression ")") cabeza-tupla-exp)
    (expression ("cola-tupla" "(" expression ")") cola-tupla-exp)
    
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
    (expression ("print" "(" expression ")") print-exp)
    (expression ("print-obj" "(" expression ")") print-obj-exp)
    (expression ("print-obj2" "(" expression "." identifier ")") print2-obj-exp)
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
     ("procedure" "(" (separated-list identifier ",") ")" "do" expression "end") procedimiento-ex)
    ;;recursiva
    (expression ("eval" expression "("(separated-list expression ",") ")"  ";" ) app-exp)
    

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

    ;;Programación Orientada a Objetos
    (class-decl                         
      ("class" identifier 
        "extends" identifier                   
         (arbno "field" identifier)
         (arbno method-decl)
         )
      a-class-decl)

    (method-decl
      ("def" identifier 
        "("  (separated-list identifier ",") ")" ; method ids
        expression 
        )
      a-method-decl)

    (expression 
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)

    (expression
      ("send" expression identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)

    (expression                                
      ("super" identifier    "("  (separated-list expression ",") ")")
      super-call-exp)
    
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
      (un-programa (c-decls body)
                   (elaborate-class-decls! c-decls)
                   (eval-expression body (init-env))))))
;init-env: ()--> <extend-env>
; función que retorna un ambiente inicial en forma de sintaxis abstracta.
; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     'var
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

;print-target: me obtiene el valor del target
;<target> -> <expval>
(define print-target
  (lambda(tgt)
    (cases target tgt
      (direct-target (expval) expval)
      (indirect-target (ref) ref))))

;eval-target: mapea una lista de targets aplicando print-target
;<list-of-targets> -> <list>
(define eval-target
  (lambda(targets)
    (map (lambda(x) (print-target x)) targets)))

;print-obj: imprime el valor de los campos del objeto
;<objeto> -> <list-of-values>
(define print-obj
  (lambda(value)
    (if (eqv? (object->super-name value) 'object)
        (cases object value
      (an-object (class-name fields) (eval-target (vector->list fields))))
        (cases object value
          (an-object (class-name fields) (aux-print-sup-obj (vector->list fields)))))))

;Funcion que ayuda a retornar solamente los valores que tienen los atributos de un objeto creado.
; <list> -> <list>
(define aux-print-sup-obj
  (lambda (fields)
    (if (null? fields)
        '()
        (if (target? (car fields))
        (cons (print-target (car fields)) (aux-print-sup-obj (cdr fields)))
        (aux-print-sup-obj (cdr fields))
        )
        )
    ))

;print-obj2: imprime el valor de un campo del objeto
;<object><list><symbol> -> <number>
(define print-obj2
  (lambda (obj obj-ids field-ids)
    (get-value-index (print-obj obj) (print-obj obj) (index-ids-obj obj-ids field-ids))))

;equal-ids-obj?:determina si el simbolo pertenece a la lista
;<list><symbol> -> <bool>
(define equal-ids-obj?
  (lambda (obj-ids field-ids)
    (if (null? obj-ids) (eopl:error "field not found ~s" field-ids)
        (if (eqv? (car obj-ids) field-ids)
            #t
            (equal-ids-obj? (cdr obj-ids) field-ids)))))

;get-value-index: Obtiene el elemento de la lista según un índice. La segunda lista es igual a la primera lista
;<list><list> -> <number>
(define get-value-index
  (lambda (lst lst2 index)
    (if (null? lst) '()
        (if(eqv?(rib-find-position (car lst) lst2) index)
       (car lst)
       (get-value-index (cdr lst) lst2 index))
        )
    ))

(define index-ids-obj
  (lambda (obj-ids field-ids)
    (if (null? obj-ids)
        '()
        (if(equal-ids-obj? obj-ids field-ids)
           (rib-find-position field-ids obj-ids)
           (index-ids-obj (cdr obj-ids) field-ids)))))


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
              (let ((args (eval-rands rands env))
                    (idss (get-decl-in-env ids env)))
                (if (null? idss)
                    (eval-expression body (extend-env 'var ids args env))
                    (if (element-in-list? (equal-type-decl idss env) #f)
                        (eopl:error 'decVar-exp "It's not possible to modify a const")   
                        (eval-expression body (extend-env 'var ids args env))))
                ))
      
      (const-exp (ids rands body)
                  (let ((args (eval-rands rands env))
                       (idss (map-const-ids ids env)))
                   (if (element-in-list? idss #t)
                        (eopl:error 'const-exp "one or more constanst were already declared before")
                        (eval-expression body (extend-env 'const ids args env))
                        )))
      
      (rec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively 'var proc-names idss bodies env)))

      ;;datos predefinidos
      (crear-lista (values) (no-empty-list (eval-regular-rands values env)))
      (listas?(exp) (lista? (eval-expression exp env)))
      (vacia-lista-exp () (empty-list))
      (lista-vacia?(exp) (empty-lista? (eval-expression exp env)))
      (cabeza-lista-exp (lst) (get-cabeza-lista (eval-expression lst env) env))
      (cola-lista-exp(lst) (get-cola-lista (eval-expression lst env)))
      (ref-lista(key lista) (get-value-lista key (eval-expression lista env) env))
      (set-lista(key value lst) (set-value-lista key (eval-expression value env) (eval-expression lst env)))
      (append-lista(lst1 lst2) (append (eval-list (eval-expression lst1 env) ) (eval-list (eval-expression lst2 env)))) 
            
      (tupla-exp(value values) (aux-crear-tupla value values))
      (tuplas?(exp) (tupla? (eval-expression exp env)))
      (ref-tupla-exp(index exp) (if (>= index 0)
                                    (get-cabeza-tupla index (eval-expression exp env) env)
                                    (eopl:error "El indice ingresado debe ser mayor o igual que cero")
                                    )) 
      (vacia-tupla-exp () (tupla-vacia))
      (cabeza-tupla-exp(exp) (get-cabeza-tupla 0 (eval-expression exp env) env))
      (cola-tupla-exp(exp) (get-cola-tupla (eval-expression exp env) env))
      (tupla-vacia?(exp) (empty-tupla? (eval-expression exp env)))
      
      (crear-registro(ids exps) (un-registro ids (eval-regular-rands exps env)))
      (registros?(exp) (registro? (eval-expression exp env)))
      (ref-registro(key registro) (get-value key (eval-expression registro env)))
      (set-registro(key value registro) (set-value key (eval-expression value env) (eval-expression registro env)))
      
      ;;Secuenciacion:
      (set-exp (id rhs-exp) (apply-decl-in-set id rhs-exp env))
      
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
      
      (for-exp (id init-value final-value body)
               (let loop ([id (eval-expression init-value env)])
                 (begin
                 (when (< id (eval-expression final-value env))
                   (eval-expression body env) 
                    (loop (+ id 1))))) )
      (print-exp (value) (print (eval-expression value env)))
      
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

      ;;Programación Orientada a Objetos
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            'initialize class-name obj args)
          obj))
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))

      (print-obj-exp (value) (print-obj (eval-expression value env)))
      (print2-obj-exp (object field) (print-obj2 (eval-expression object env) (object->field-ids (eval-expression object env)) field))
      ;(print2-obj-exp (object field) (eopl:error "a ~s" field))
      )))


; Declaraciones POO
(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))
; Fin declaraciones POO


(define print
  (lambda(value)
    (display value)
    '-))

(define cabeza-tupla
  (lambda (tp1)
    (cases tupla tp1
      (una-tupla (tp1) #t)
      (else #f))))

(define aux-crear-tupla
  (lambda (valor lista)
    (una-tupla (append (list valor) lista))))

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

(define empty-lista? (lambda(lst) (cases lista lst (empty-list () #t) (else #f) )))

;map-const-ids: función auxiliar que hace un mapeo de una lista de elementos verificando si se encuentra uno en un ambiente.
;<listae> <environment> -> <lista>
(define map-const-ids
  (lambda (elem env)
    (map (lambda (x) (element-in-env? x env)) elem)))

;equal-type-decl: función auxiliar que hace un mapeo verificando si la declaración que recibe es de tipo var.
(define equal-type-decl
  (lambda (elem env)
    (map (lambda (x) (equal? (type-decl x env) 'var)) elem)))

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
    (cases expression rand
      (identificador (id)
                     (let ((ref (apply-env-ref env id)))
                       (let ((ref-to-value (cases target (primitive-deref ref)
                                             (direct-target (expval) ref)
                                             (indirect-target (ref1) ref1))))
                         (target-redirect (primitive-deref ref-to-value) ref-to-value)
                         )
                       ))
      (else
       (direct-target (eval-expression rand env))))))

(define target-redirect
  (lambda(tar ref)
    (cases target tar
      (direct-target(expval)(cond
                              [(registro? expval) (indirect-target ref)]
                              [(lista? expval) (indirect-target ref)]
                              [else (direct-target expval) ])
      )
      (indirect-target "eopl: It is not expected an indirect-target as value")
    )))


;eval-regular-rands: Funcion cuya finalidad es evaluar ids sin realizar la creacion de targets.
(define eval-regular-rands
  (lambda(rands env)
    (map (lambda(x)(eval-regular-rand x env)) rands)))

(define eval-regular-rand
  (lambda(rand env)
    (eval-expression rand env)))

;apply-set-exp: evalua el cuerpo de un set en el ambiente correspondiente
(define apply-set-exp
  (lambda (id rhs-exp env)
    (begin
      (setref!
       (apply-env-ref env id)
       (eval-expression rhs-exp env))
      1)))

;apply-decl-in-set: aplica el set dependiendo del tipo de declaración definida (const/var)
(define apply-decl-in-set
  (lambda (id rhs-exp env)
    (if (element-in-env? id env)
        (let ((type (type-decl id env)))
          (if (equal? type 'var)
              (apply-set-exp id rhs-exp env)
              (eopl:error 'set-exp "It's not possible to modify a const")))
        (eopl:error 'set-exp "No binding for ~s" id))))



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
;-----------------------Estructuras de datos (listas)(registro)(tuplas) y funciones-------------
(define-datatype lista lista?
  (empty-list)
  (no-empty-list (lst (list-of expval?))))

(define get-cabeza-lista
  (lambda(lst env)
    (cases lista lst
      (empty-list() (eopl:error "Está intentando obtener un elemento de una lista vacío"))
      (no-empty-list(first ) (car first ))
      )
    )
  )

(define get-cola-lista
  (lambda(lst)
    (cases lista lst
      (empty-list() (eopl:error "Está intentando obtener un elemento de una lista vacío"))
      (no-empty-list(lst) (tail-lista lst))
      )
    )
  )
(define tail-lista
  (lambda(lst)
    (cond
      [(eqv? lst '()) (eopl:error "Está intentando obtener la cola de una lista vacía")]
      [(eqv? (cdr lst) '()) (car lst)]
      [else (tail-lista (cdr lst))]
      )
    ))

(define get-value-lista
  (lambda(index lst env)
    (cases lista lst
      (empty-list() (eopl:error "Está intentando obtener un elemento de una lista vacío"))
      (no-empty-list(exps) (search-value-lista index  exps ) )
      )
    )
  )

(define search-value-lista
  (lambda(index exps)
    (cond
      [(eqv? exps '()) (eopl:error "Está intentando obtener un elemento de una lista vacío")]
      [(eqv? index 0)  (car exps)]
      [else (search-value-lista (- index 1) (cdr exps))]
      ))
  )

(define set-value-lista
  (lambda(index value lst)
    (cases lista lst
      (empty-list() (eopl:error "Está intentando obtener un elemento de una lista vacío"))
      (no-empty-list(exps) (no-empty-list (set-index (list->vector exps ) index value)))
      )
    )
  )
(define eval-list
  (lambda(lst )
    (cases lista lst
      (empty-list()'())
      (no-empty-list(exps) exps)      
      )  
    ))
(define concatena (lambda (X Y)
                    (if (null? X)
                        Y
                        (cons (car X) (concatena (cdr X) Y))
                    )
                    ))


(define-datatype tupla tupla?
  (tupla-vacia)
  (una-tupla (exps (list-of expression?))))

(define get-cola-tupla
  (lambda(tp1 env)
    (cases tupla tp1
      (tupla-vacia() (eopl:error "Está intentando obtener un elemento de una tupla vacío"))
      (una-tupla(exps) (search-cola-tupla exps env))
      )
    )
  )

(define search-cola-tupla
  (lambda(exps env)
    (cond
      [(null? (cdr exps)) (eval-expression (car exps) env)]
      ;[(eqv? index 0) (eval-expression (car exps) env)]
      [else (search-cola-tupla (cdr exps) env)]
      ))
  )

(define get-cabeza-tupla
  (lambda(index tp1 env)
    (cases tupla tp1
      (tupla-vacia() (eopl:error "Está intentando obtener un elemento de una tupla vacío"))
      (una-tupla(exps) (search-value-tupla index exps env))
      )
    )
  )

(define search-value-tupla
  (lambda(index exps env)
    (cond
      [(eqv? index '()) #t]
      [(eqv? index 0) (eval-expression (car exps) env)]
      [else (search-value-tupla (- index 1) (cdr exps) env)]
      ))
  )

(define-datatype registro registro?
  (vacio)
  (un-registro (ids (list-of symbol?)) (exps (list-of expval?))))

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
               (eval-expression body (extend-env 'var ids args env))))))

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
   (decl symbol?)
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
  (lambda (type-decl syms vals env)
    (extended-env-record type-decl syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (type-decl proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record type-decl proc-names vec old-env)))
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
      (extended-env-record (decl syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
;type-decl:función que retorna el tipo de la declaración.
;<symbol> <environment> -> <symbol>
(define type-decl
  (lambda (decln env)    
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'type-decl "No binding for ~s" decln))
      (extended-env-record (decl syms vals env)
                           (if (element-in-list? syms decln)
                               decl
                               (type-decl decln env)))
     )))

;element-in-env?: función que verifica si un elemento se encuentra en un ambiente
;<symbol> <environment> -> <boolean>
(define element-in-env?
  (lambda (elem env)
    (cases environment env
      (empty-env-record ()
                        #f)
      (extended-env-record (decl syms vals env)
                           (let ((ids syms))
                               (if (element-in-list? ids elem)
                                   #t
                                   (element-in-env? elem env)
                                )))
                        )))

;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record 'var syms vec env)))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))

;--------------- Clases ----------------;
(define-datatype class class?
  (a-class
    (class-name symbol?)  
    (super-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)))

;------------- Construcción de clases -----------------;
(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))

(define elaborate-class-decl!
  (lambda (c-decl)
    (let ((super-name (class-decl->super-name c-decl)))
      (let ((field-ids  (append
                          (class-name->field-ids super-name)
                          (class-decl->field-ids c-decl))))
        (add-to-class-env!
          (a-class
            (class-decl->class-name c-decl)
            super-name
            (length field-ids)
            field-ids
            (roll-up-method-decls
              c-decl super-name field-ids)))))))

(define roll-up-method-decls
  (lambda (c-decl super-name field-ids)
    (map
      (lambda (m-decl)
        (a-method m-decl super-name field-ids))
      (class-decl->method-decls c-decl))))

;^;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

;^; an object is now just a single part, with a vector representing the
;^; managed storage for the all the fields. 

(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (make-vector (class-name->field-length class-name))))) ;\new1

;^;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

(define-datatype method method?
  (a-method
    (method-decl method-decl?)
    (super-name symbol?)
    (field-ids (list-of symbol?))))

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let loop ((host-name host-name))
      (if (eqv? host-name 'object)
          (eopl:error 'find-method-and-apply
            "No method for name ~s" m-name)
          (let ((method (lookup-method m-name ;^ m-decl -> method
                          (class-name->methods host-name))))
            (if (method? method)
                (apply-method method host-name self args)
                (loop (class-name->super-name host-name))))))))

(define apply-method
  (lambda (method host-name self args)                ;\new5
    (let ((ids (method->ids method))
          (body (method->body method))
          (super-name (method->super-name method))
          (field-ids (method->field-ids method))       
          (fields (object->fields self)))
      (eval-expression body
        (extend-env
         'var
          (cons '%super (cons 'self ids))
          (if (eqv? super-name '@object)
              (cons super-name (cons (direct-target self) args))
              (cons (direct-target super-name) (cons (direct-target self) args)))
          (extend-env-refs field-ids fields (empty-env)))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

(define method-environment? (list-of method?)) 

(define lookup-method                   
  (lambda (m-name methods)
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))

;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of classes (not class decls)

(define the-class-env '())

(define initialize-class-env!
  (lambda ()
    (set! the-class-env '())))

(define add-to-class-env!
  (lambda (class)
    (set! the-class-env (cons class the-class-env))))

(define lookup-class                    
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) (eopl:error 'lookup-class
                       "Unknown class ~s" name))
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define class->class-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        class-name))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        super-name))))

(define class->field-length
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-length))))

(define class->field-ids
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-ids))))

(define class->methods
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        methods))))

(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
        class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
        fields))))

(define object->field-length
  (lambda (object)
    (class->field-length
      (object->class-decl object))))

(define object->class-decl
  (lambda (obj)
    (lookup-class (object->class-name obj))))

(define object->field-ids
  (lambda (object)
    (class->field-ids
      (object->class-decl object))))

(define object->super-name
  (lambda (object)
    (class->super-name
      (object->class-decl object))))

(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->field-ids (lookup-class class-name)))))

(define class-name->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->methods (lookup-class class-name)))))

(define class-name->field-length
  (lambda (class-name)
    (if (eqv? class-name 'object)
        0
        (class->field-length (lookup-class class-name)))))

(define method->method-decl
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) meth-decl))))

(define method->super-name
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) super-name))))

(define method->field-ids
  (lambda (meth)
    (cases method meth
      (a-method (method-decl super-name field-ids) field-ids))))

(define method->method-name
  (lambda (method)
    (method-decl->method-name (method->method-decl method))))

(define method->body
  (lambda (method)
    (method-decl->body (method->method-decl method))))

(define method->ids
  (lambda (method)
    (method-decl->ids (method->method-decl method))))


;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (tupla? x) (registro? x) (boolean? x) (string? x) (class? x) (symbol? x) (object? x) (method? x) (lista? x))))

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
    (if(target? ref)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval))) (primitive-setref! ref (direct-target expval)))))

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
;<list> <scheme-value> -> <bool>
(define element-in-list?
  (lambda (lst elem)
    (cond
      ((null? lst) #f)
      ((equal? (car lst) elem) #t)
      (else (element-in-list? (cdr lst) elem)))
       ))

;get-decl-in-env: función auxiliar que me obtiene enlistado los elementos que se encuentran en un ambiente
;<list> <environment> -> <list>
(define get-decl-in-env
  (lambda (lst env)
    (cond
    ((null? lst) '())
    ((element-in-env? (car lst) env) (cons (car lst) (get-decl-in-env (cdr lst) env)))
    (else (get-decl-in-env (cdr lst) env)))
    ))

;(define get-id-in-env
;  (lambda (ids env)
;    (cond
;      ((null? ids) '())
;      ((element-in-env? lst env)))))

(show-the-datatypes)
just-scan
scan&parse

;;Ejemplos de scan&parse
;;Ejemplo paso por referencia registros:
(scan&parse "var w = crear-registro({j=0}) in var p = procedure() do
begin set w = set-registro(j, (ref-registro(j, w)+10), w);
set y = ref-registro(j, w); y end end in begin eval p();; eval p(); end")

;;Ejemplo paso por valor de cualquier otro dato:
(scan&parse "var x=100, funct = procedure (a) do
 begin set a = add1(a); a end end in (eval funct(x); + eval funct(x);)")

;numero
(scan&parse "3")
;identificador
(scan&parse "x")
;begin-exp
(scan&parse "begin (3 +2); 2 end")
;tupla
(scan&parse "crear-tupla(tupla[1;2;true])")
(scan&parse "tupla?(crear-tupla(tupla[1;2;true]))")
(scan&parse "vacio-tupla?(crear-tupla(tupla[1;2;true]))")
(scan&parse "vacio-tupla()")
(scan&parse "vacio-tupla?(vacio-tupla())")
(scan&parse "cabeza-tupla(crear-tupla(tupla[1;2;true]))")
(scan&parse "cola-tupla(crear-tupla(tupla[1;2;true]))")
;set-exp
(scan&parse "set x=3")
;condicional-exp
(scan&parse "if and(true, false) then 1 [else 0] end")
;lista:
(scan&parse "begin
   set x = crear-lista([3;2]);
   cabeza-lista(x);
   cola-lista(x);
   vacio-lista?(x);
   set y = vacio-lista();
   lista?(y)
end")
;paso por referencia lista
(scan&parse "var w = crear-lista([1;2;3;4]) in var p = procedure() do
begin set w = set-lista(0, (ref-lista(0, w)+10), w);
set y = ref-lista(0, w); y end end in begin eval p();; eval p(); end")

(scan&parse " append (crear-lista([1;2;3;4]),crear-lista([1;2;3;4]))")
;lista ref and set
(scan&parse "begin
    set x = crear-lista([1;2;3;4]);
    if lista?(x) then set z = set-lista(0, 5, x)
       [else 0] end;  
    set y = ref-lista(1,  crear-lista([6;7;8;9]));
    y
end")
(scan&parse "append (crear-lista([1;2;3;4]),crear-lista([1;2;3;4]))")
;registros:
(scan&parse "begin
    set x = crear-registro({y=3});
    if registros?(x) then set z = set-registro(y, 5, x)
       [else 0] end;  
    set y = ref-registro(w,  crear-registro({w=18}));
    y
end")
; rec-exp y app-exp
(scan&parse "rec 
     factorial(n) = 
       if ==(n, 0) then 1
      [else (n * eval factorial(sub1(n));)] end
    in
     eval factorial(4);")
; 
;while-exp
(scan&parse "begin while <(x,10) do set x=(x+1) done; x end")
;for-exp
(scan&parse "begin for i=0 to 10 do set x= 3  done; x end")
;decVar-exp
(scan&parse "var n = 5 in n")
;const-exp
(scan&parse "const u = 5 in u")
;******************************************************************************************
;Parte 2 - Programación Orientada a Objetos
;creación de objetos
(scan&parse "class cl extends object field x def initialize() begin set x = 1 end def m1() x var o1 = new cl() in send o1 m1()")
;invocación de métodos y selección de campos
(scan&parse "class cl extends object field x def initialize() begin set x = 1 end def m1() x var o1 = new cl() in send o1 m1()")
;Print objetos, incluidos objetos con herencia y superclase
(scan&parse "class c1 extends object  field x field y  def initialize()  begin   set x = 1; set y = 2 end def m1() x def m2() y  class c2 extends c1  field x field y  def initialize()  begin   super initialize(); set  x = 2; set y = 3 end def m1() x class c3 extends c2  field x field y  def initialize()  begin   super initialize(); set  x = 200; set y = 300 end def m1() x  var o1 = new c1(), o2 = new c2(), o3 = new c3() in print-obj(o3)")
;Print un campo del objeto
(scan&parse "class c1 extends object  field x field y  def initialize()  begin   set x = 1; set y = 2 end def m1() x def m2() y  class c2 extends c1  field x field y  def initialize()  begin   super initialize(); set  x = 2; set y = 3 end def m1() x class c3 extends c2  field x field y  def initialize()  begin   super initialize(); set  x = 200; set y = 300 end def m1() x  var o1 = new c1(), o2 = new c2(), o3 = new c3() in print-obj2(o3.y)")
;Super clase
(scan&parse "class c1 extends object  field x field y  def initialize()  begin   set x = 1; set y = 2 end def m1() x def m2() y  class c2 extends c1  field x field y  def initialize()  begin   super initialize(); set  x = 2; set y = 3 end def m1() x class c3 extends c2  field x field y  def initialize()  begin   super initialize(); set  x = 200; set y = 300 end def m1() x var o1 = new c1(), o2 = new c2(), o3 = new c3() in send o3 m1()")
;Atualización de campos
(scan&parse "class c1 extends object  field x field y  def initialize()  begin   set x = 1; set y = 2 end def m1() x def m2() y  class c2 extends c1  field x field y  def initialize()  begin   super initialize(); set  x = 2; set y = 3 end def m1() x class c3 extends c2  field x field y  def initialize()  begin   super initialize(); set  x = 200; set y = 300 end def m1() x def suma(a, b) begin set x=(a+b); x end var o1 = new c1(), o2 = new c2(), o3 = new c3() in send o3 suma(40, 30)")
;Herencia
(scan&parse "class c1 extends object  field x field y  def initialize()  begin set x = 1; set y = 2 end def m1() x def m2() y  class c2 extends c1  field x field y  def initialize()  begin set x = 2; set y = 3 end def m1() x  var o1 = new c1(), o2 = new c2() in send o2 m2()")
;Polimorfismo
(scan&parse "class c1 extends object  field x field y  def initialize()  begin set x = 1; set y = 2 end def m1() x def m2() y  class c2 extends c1  field x field y  def initialize()  begin set x = 2; set y = 3 end def m1() 5  var o1 = new c1(), o2 = new c2() in send o2 m1()")
