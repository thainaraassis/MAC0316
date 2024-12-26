#lang plai-typed

#|
 | interpretador simples, sem variáveis ou funçõess
 |#

#| primeiro as expressões "primitivas", ou seja, diretamente interpretadas
 |#

(define-type ExprC
  [numC    (n : number)]
  [idC     (s : symbol)]
  [plusC   (l : ExprC) (r : ExprC)]
  [multC   (l : ExprC) (r : ExprC)]
  [lamC    (arg : symbol) (body : ExprC)]
  [appC    (fun : ExprC) (arg : ExprC)]
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [consC   (car : ExprC) (cdr : ExprC)]; Creates cell with a pair
  [carC    (pair : ExprC)]; Gets 1st element of a pair
  [cdrC    (pair : ExprC)]; Gets 2nd element of a pair

  ;; new implementation
  [letrecC (s : symbol) (v : ExprC) (r : ExprC)]
  [quoteC  (s : symbol)]
  [read-loopC]
  ;;
  )
#| agora a linguagem aumentada pelo açúcar sintático
 | neste caso a operação de subtração e menus unário
 |#

(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [consS   (car : ExprS) (cdr : ExprS)]
  [carS    (pair : ExprS)]
  [cdrS    (pair : ExprS)]

  ;; new implementation
  [letS    (s : symbol) (v : ExprS) (r : ExprS)]
  [let*S   (s1 : symbol) (v1 : ExprS) (s2 : symbol) (v2 : ExprS) (r : ExprS)]
  [letrecS (s : symbol) (v : ExprS) (r : ExprS)]
  [quoteS  (s : symbol)]
  [read-loopS]
  ;;
  )


(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [lamS    (a b)      (lamC a (desugar b))]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [consS   (b1 b2)    (consC (desugar b1) (desugar b2))]
    [carS    (c)        (carC (desugar c))]
    [cdrS    (c)        (cdrC (desugar c))]

    ;; new implementation
    [letS    (s v r)    (appC (lamC s (desugar r)) (desugar v))]
    [let*S   (s1 v1 s2 v2 r)  (appC (lamC s1 (appC (lamC s2 (desugar r)) (desugar v2))) (desugar v1))]
    [letrecS (s v r)    (letrecC s (desugar v) (desugar r))]
    [quoteS  (s)        (quoteC s)]
    [read-loopS () (read-loopC)]
    ;;
    ))



; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [consV (car : Value) (cdr : Value)]

  ;; new value
  [symV  (s : symbol)]
  
  )


; Bindings associate symbol with Boxes
; we need this to be able to change the value of a binding, which is important
; to implement letrec.

(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])


; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


; Storage's operations are similar to Env's
;   bind <-> cell
;   mt-env <-> mt-store
;   extend-env <-> override-store


; lookup changes its return type
(define (lookup [varName : symbol] [env : Env]) : (boxof Value); lookup returns the box, we need this to change the value later
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string varName) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                    [(symbol=? varName (bind-name (first env)))   ; achou!
                     (bind-val (first env))]
                    [else (lookup varName (rest env))])]))        ; vê no resto



; Primitive operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))


; Return type for the interpreter, Value


(define (interp [a : ExprC] [env : Env] ) : Value
  (type-case ExprC a
    [numC (n) (numV n) ]
    [idC (n)  (unbox (lookup n env))]; we need to unbox the value in the environment before using it
    [lamC (a b) (closV a b env) ]

 
    ; application of function
    [appC (f a)
          (let ((closure (interp f env))
                (argvalue (interp a env)))
            (type-case Value closure
              [closV (parameter body env)
                     (interp body (extend-env (bind parameter (box argvalue)) env))]
              [else (error 'interp "operation app aplied to non-closure")]
              ))]
   
    ;I left plusC without error-checking
    [plusC (l r)
             (let ((left (interp l env))
                   (right (interp r env)))
               (num+ left right))]
    ;multC
    [multC (l r)
           (let ( (left (interp l env))
                  (right (interp r env)))
             ;in this case type cheking is a little different
             (if (numV? left)
                 (if (numV? right)
                     (num* left right)
                     (error 'interp "second argument of multiplication not a number value"))
                 (error 'interp "first argument of multiplication not a number value"))
                 )]
    ; ifC serializes
    [ifC (c s n) (type-case Value (interp c env)
                   [numV (value)
                        (if (zero? value)
                            (interp n env )
                            (interp s env ))]
                   [else (error 'interp "condition not a number")]
                   )]

    ; Working with lists
    [consC (b1 b2) (let ( (car (interp b1 env))
                          (cdr (interp b2 env)))
                     (consV car cdr))]
    [carC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       car]
                [else (error 'interp "car applied to non-cell")]
                )]
    [cdrC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       cdr]
                [else (error 'interp "cdr applied to non-cell")]
                )]

    ;; new implementation
    [letrecC (name val body)
             (let* ([new-box (box (numV 0))]
                    [fecha (interp val (extend-env (bind name new-box) env))])
                   (type-case Value fecha
                     [closV (arg2 body2 env) (let ([dummy (set-box! (lookup name env) fecha)]) (interp body env))]
                     [else (error 'interp "nothing")]))]
    [quoteC (s) (symV s)]
  
    [read-loopC () (letrec ((read-till-end 
                             (lambda () 
                               (let ((input (read))) 
                                 (if (and (s-exp-symbol? input) (eq? (s-exp->symbol input) '@end))
                                     (begin (display "FINISHED-READLOOP") (numV 0))  
                                     (begin (display "\ninterpret-command:")
                                            (display input) 
                                            (display "\nresult:")
                                            (display (interpS input))  
                                            (display "\n")
                                            (numV 0)
                                            (read-till-end)))))))  
                     (read-till-end))] 
    ;;
    )) 


; Parser with funny instructions for boxes
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(car) (carS (parse (second sl)))]
         [(cdr) (cdrS (parse (second sl)))]
         
         ;; new implementation
         [(let) (letS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                      (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                      (parse (third sl)))]
         [(let*) (let*S (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                        (parse (second (s-exp->list (first(s-exp->list (second sl))))))
                        (s-exp->symbol (first (s-exp->list (second ( s-exp->list (second sl))))))
                        (parse (second (s-exp->list (second (s-exp->list (second sl))))))
                        (parse (third sl)))]
         [(letrec) (letrecS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                            (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                            (parse (third sl)))]
         [(quote) (quoteS (s-exp->symbol (second sl)))] ;; gets the second element of the list, which comes after quote and converts into a symbol
         [(read-loop) (read-loopS)] 
         ;;
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))


; Examples
(interpS '(+ 10 (call (lambda x (car x)) (cons 15 16))))

(interpS '(call (lambda x (+ x 5)) 8))


(interpS '(call (lambda f (call f (~ 32))) (lambda x (- 200 x))))

 
; Tests
(test (interp (carC (consC (numC 10) (numC 20)))
              mt-env)
      (numV 10))

; Test for let
(test (interpS '(let ((x 1)) (let ((y 2)) (+ x y)))) (numV 3))
(test (interpS '(let ((x 2)) (let ((y 3)) (+ x y)))) (numV 5))
(test (interpS '(let ((x 5)) x)) (numV 5))
(test (interpS '(let ((x 10)) (let ((x 20))x))) (numV 20))
(test (interpS '(let ([x 3]) x)) (numV 3))
(test (interpS '(let ((x 5)) (+ x 3))) (numV 8))
(test (interpS '(let ((x 2)) (* 3 x))) (numV 6))
(test (interpS '(let ([x 10]) (+ x x))) (numV 20))

; Test for let*
(test (interpS '(let* ((x 5) (y (+ x 10))) (+ x y))) (numV 20))
(test (interpS '(let* ((x 1) (z (+ x 2)))z) ) (numV 3))
(test (interpS '(let* ((a 3) (b (* a 2))) (+ a b))) (numV 9))
(test (interpS '(let* ((x 5) (y (+ x 1))) (+ y 2))) (numV 8))
(test (interpS '(let* ([x 1] [y 2]) (+ x y))) (numV 3))
(test (interpS '(let* ([x 2] [y x]) (+ x y))) (numV 4))
(test (interpS '(let* ((x 5) (y (+ x 2))) (+ x y))) (numV 12))
(test (interpS '(let* ([x 10]  [y 20]) (+ x y))) (numV 30))
 
; Test for letrec
(test (interpS '(letrec ([fac (lambda n (if n (+ n (call fac (- n 1))) 1))]) (call fac 5))) (numV 16))
(test (interpS '(letrec ([f (lambda n n)]) (call f 3))) (numV 3))
(test (interpS '(letrec ([fact (lambda n (if n (* n (call fact (- n 1))) 1))]) (call fact 5))) (numV 120)) 

; Test for quote
(test (interpS '(quote alan)) (symV 'alan))
(test (interpS '(quote abc)) (symV 'abc))

; More Test (monitor)
(test (interpS '(let ((x 100)) x)) (numV 100))
(test (interpS '(let ((x 100)) (+ 1 x))) (numV 101))
(test (interpS '(let ((x 100)) (let ((y (+ x 1))) (+ x y)))) (numV 201))
(test (interpS '(let ((f (lambda x x))) (call f 10))) (numV 10))
(test (interpS '(let ((plus (lambda x (lambda y (+ x y)))))
    (call (call plus 10) 20) )) (numV 30))
(test (interpS '(let* ((x 100) (y (+ x 1))) (+ x y))) (numV 201))
(test (interpS 
    '(let* ((inc (lambda n (+ n 1))) (inc2 (lambda n (call inc (call inc n))))) (call inc2 10)))
    (numV 12))
(test (interpS '(letrec ((f (lambda n (if n (* 2 (call f (- n 1))) 1)))) (call f 10))) (numV 1024))
(test (interpS '(letrec ((f (lambda n (if n (* 2 (call f (- n 1))) 1)))) 10)) (numV 10))
(test (interpS '(letrec ((f (lambda n n))) (call f 7))) (numV 7))

; Test for read-loop
(interpS '(read-loop))
