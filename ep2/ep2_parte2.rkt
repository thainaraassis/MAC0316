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
  [letrecC (s : symbol) (v : ExprC) (r : ExprC)]
  [quoteC  (s : symbol)]
  [read-loopC]

  ;; new implementation
  [equal?C (l : ExprC) (r : ExprC)]
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
  [letS    (s : symbol) (v : ExprS) (r : ExprS)]
  [let*S   (s1 : symbol) (v1 : ExprS) (s2 : symbol) (v2 : ExprS) (r : ExprS)]
  [letrecS (s : symbol) (v : ExprS) (r : ExprS)]
  [quoteS  (s : symbol)]
  [read-loopS]
  
  ;; new implementation
  [equal?S (l : ExprS) (r : ExprS)]
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
    [letS    (s v r)    (appC (lamC s (desugar r)) (desugar v))]
    [let*S   (s1 v1 s2 v2 r)  (appC (lamC s1 (appC (lamC s2 (desugar r)) (desugar v2))) (desugar v1))]
    [letrecS (s v r)    (letrecC s (desugar v) (desugar r))]
    [quoteS  (s)        (quoteC s)]
    [read-loopS () (read-loopC)]

    ;; new implementation
    [equal?S (l r)      (equal?C (desugar l) (desugar r))]
    ;;
    ))


;; new implementation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Promise
    [aPromise (valueBox : (boxof Value))])

(define (query-promise [prom : Promise]) : Value
        (let ((theBox (aPromise-valueBox prom)))
          (type-case Value (unbox theBox)
            [suspV (body susp-env)
                  (let* ((finalValue (interp body susp-env)))
                    (begin (set-box! theBox finalValue)
                           finalValue))]
            [else (unbox theBox)]
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [symV  (s : symbol)]

  ;; new value
  [consV (car : Promise) (cdr : Promise)]
  [suspV (body : ExprC) (env : Env)]
  
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
    [idC (n)  (query-promise (aPromise (lookup n env)))]; 
    [lamC (a b) (closV a b env) ]

 
    ; application of function
    [appC (f a)
          (let ((closure (interp f env))
                (argvalue (suspV a env)))
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
    [consC (b1 b2) (let ( (car (box (suspV b1 env)))
                          (cdr (box (suspV b2 env))) )
                     (consV (aPromise car) (aPromise cdr)))]
    
    [carC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       (query-promise car)]
                [else (error 'interp "car applied to non-cell")]
                )]
    
    [cdrC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       (query-promise cdr)]
                [else (error 'interp "cdr applied to non-cell")]
                )]
    
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

    ;; new implementation
    [equal?C (l r)
      (if (equal? (query-promise (aPromise (box (interp l env)))) (query-promise (aPromise (box (interp r env)))))
          (numV 1)
          (numV 0))]
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
         [(quote) (quoteS (s-exp->symbol (second sl)))]

         ;; new implementation
         [(equal?) (equal?S (parse (second sl)) (parse (third sl)))]
         ;;
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))


; Examples
(interpS '(+ 10 (call (lambda x (car x)) (cons 15 16))))

(interpS '(call (lambda x (+ x 5)) 8))


(interpS '(call (lambda f (call f (~ 32))) (lambda x (- 200 x))))

 
;; Testes do EP1

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

;; Testes do EP2

(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (let ((death (call halt 0))) 1))) 
      (numV 1))
(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (car (cons 2 (call halt 0))))) 
      (numV 2))
(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (cdr (cons (call halt 0) 3)))) 
      (numV 3))
(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (let ((first (lambda x (lambda y x))))
                    (call (call first 4) (call halt 0))))) 
      (numV 4))
(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (let ((second (lambda x (lambda y y))))
                    (call (call second (call halt 0)) 5)))) 
      (numV 5))
(test (interpS '(letrec ((iterate (lambda f (lambda x (cons x (call (call iterate f) (call f x)))))))
                 (let ((things (call (call iterate (lambda x (* 10 x))) 1)))

                   (+ (+ (car things) (car (cdr things)))
                      (car (cdr (cdr things)))))

                 )) 
      (numV 111))
(test (interpS '(letrec ((drop (lambda n (lambda xs (if n (call (call drop (- n 1)) (cdr xs)) xs)))))
                  (letrec ((iterate (lambda f (lambda x (cons x (call (call iterate f) (call f x)))))))
                    (let ((things (call (call drop 3) (call (call iterate (lambda x (* 10 x))) 1))))

                      (+ (+ (car things) (car (cdr things)))
                         (car (cdr (cdr things)))))
                    
                    ))) 
      (numV 111000))
