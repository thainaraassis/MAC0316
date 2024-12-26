#lang plai-typed

#|
 | Incluindo troca de sinal: - unário, uminus
 | Mais açúcar?
 |#

(define-type ArithC
  [numC (n : number)]
  [boolC (b : boolean)]
  [andC (l : ArithC) (r : ArithC)]  
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [lessC (l : ArithC) (r : ArithC)]
  [ifC  (c : ArithC)  (t : ArithC) (e : ArithC)]
  ;
  ; NOVAS OPERAÇÕES
  ;
  [divC   (l : ArithC) (r : ArithC)]  ; divisão
  [orC    (l : ArithC) (r : ArithC)]  ; or
  [equalC (l : ArithC) (r : ArithC)]  ; igualdade
  [notC   (n : ArithC)])              ; not


; Incluindo o sinal negativo
(define-type ArithS
  [numS    (n : number)]
  [boolS   (b : boolean)]
  [andS    (l : ArithS) (r : ArithS)]
  [plusS   (l : ArithS) (r : ArithS)]
  [multS   (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [lessS   (l : ArithS) (r : ArithS)]
  [ifS  (c : ArithS) (t : ArithS) (e : ArithS)]
  ;
  ; NOVAS OPERAÇÕES
  ;
  [divS     (l : ArithS) (r : ArithS)]  ; divisão
  [orS      (l : ArithS) (r : ArithS)]  ; or
  [equalS   (l : ArithS) (r : ArithS)]  ; igualdade
  [notS     (n : ArithS)]               ; not
  [uminusS  (n : ArithS)]               ; menos unitario
  [greaterS (l : ArithS) (r : ArithS)]) ; maior


; Nova "desugar", ou quase

(define (desugar [as : ArithS]) : ArithC  
  (type-case ArithS as
    [numS    (n)   (numC n)]
    [boolS   (b)   (boolC b)]
    [andS    (l r) (andC (desugar l) (desugar r))]
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [lessS  (l r)  (lessC (desugar l) (desugar r))]
    [ifS (c t e) (ifC (desugar c) (desugar t) (desugar e))]
    ;
    ; NOVAS OPERAÇÕES
    ;
    [divS   (l r) (divC (desugar l) (desugar r))]     ; divisão
    [orS    (l r) (orC  (desugar l) (desugar r))]     ; or
    [equalS (l r) (equalC  (desugar l) (desugar r))]  ; igualdade
    [notS   (n)   (notC (desugar n))]                 ; not
    ;
    ;Abaixo todos os operadores que são açúcar sintático
    ; 
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    ;
    ; NOVOS OPERADORES
    ;
    [uminusS  (n) (multC (numC -1) (desugar n))]     ; menos unário
    [greaterS (l r) (andC (notC(lessC (desugar l) (desugar r))) (notC(equalC  (desugar l) (desugar r))))])) ; maior
    ;

; O interpretador é o mesmo, pois no final ainda temos ArithC
(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC   (n) n]
    [boolC  (b) (if b 1 0)]
    [andC  (l r)(if (and (not (=  (interp l) 0)) (not (= (interp r) 0))) 1 0)]
    [plusC  (l r) (+ (interp l) (interp r))]
    [multC  (l r) (* (interp l) (interp r))]
    [lessC  (l r) (if (< (interp l) (interp r)) 1 0)]
    [ifC    (c t e) (if (not (= (interp c) 0))
                        (interp t)
                        (interp e))]
    ;
    ; NOVAS OPERAÇÕES
    ;
    [divC   (l r) (/ (interp l) (interp r))]                                    ; divisão
    [orC    (l r) (if (or (not (=  (interp l) 0)) (not (= (interp r) 0))) 1 0)] ; or
    [equalC (l r) (if (= (interp l) (interp r)) 1 0)]                           ; igualdade
    [notC   (n)   (if (= (interp n) 0) 1 0)]))                                  ; not
    ;

; o parser muda mais um pouco
(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-boolean? s) (boolS (s-exp->boolean s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(and) (andS (parse (second sl)) (parse (third sl)))]
         [(<) (lessS  (parse (second sl)) (parse (third sl)))]
         [(if)(ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         ;
         ;
         [(/) (divS (parse (second sl)) (parse (third sl)))]      ; divisão
         [(or) (orS (parse (second sl)) (parse (third sl)))]      ; or
         [(=) (equalS (parse (second sl)) (parse (third sl)))]    ; igualdade
         [(not) (notS (parse (second sl)))]                       ; not
         [(~) (uminusS (parse (second sl)))]                      ; menor unário
         [(>) (greaterS  (parse (second sl)) (parse (third sl)))] ; maior
         ;
         ;
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (interpS [ s : ArithS] ) : number
  (interp (desugar s)))


; teste divisão
;(display (interpS (parse '(/ 8 2))))  ; 4
;(display (interpS (parse '(/ 9 3))))  ; 3
;(display (interpS (parse '(/ 10 2)))) ; 5

; teste or
;(display (interpS (parse '(or 0 0))))
;(display (interpS (parse '(or 0 1))))
;(display (interpS (parse '(or 1 0))))
;(display (interpS (parse '(or 1 1))))

; teste igualdade
;(display (interpS (parse '(= 5 5))))
;(display (interpS (parse '(= 5 6))))
;(display (interpS (parse '(= (+ 3 2) 5))))
;(display (interpS (parse '(= (- 7 2) 5))))
;(display (interpS (parse '(= (* 2 3) 6))))
;(display (interpS (parse '(= (* 2 3) 7))))
;(display (interpS (parse '(= (and (= 1 1) (= 2 2)) 1))))
;(display (interpS (parse '(= (or (= 1 1) (= 2 2)) 1))))

; teste not
;(display (interpS (parse '(not (= 5 5)))))
;(display (interpS (parse '(not (= 5 6)))))
;(display (interpS (parse '(not (and (= 1 0) (= 2 2))))))
;(display (interpS (parse '(not (or (= 1 0) (= 2 2))))))

; teste menos unário
;(display (interpS (parse '(~ 5))))
;(display (interpS (parse '(~ (~ 3)))))
;(display (interpS (parse '(~ (+ 4 6)))))
;(display (interpS (parse '(~ (- 10 7)))))

; testa maior
;(display (interpS (parse '(> 3 2))))
;(display (interpS (parse '(> 2 3))))
;(display (interpS (parse '(> (+ 5 5) 8))))
;(display (interpS (parse '(> (- 10 5) 4))))
;(display (interpS (parse '(> 4 4))))

; -------- TESTES DO PROFESSOR --------

(display (interpS (parse '(+ 5 (~ 3)))))
(display "\n")
  
(display (interpS (parse '(if (= (+ (~ 5) 7) (+ 1 1)) (+ 8 2) (/ 4 2)))))
(display "\n")
  
(display (interpS (parse '#t)))
(display "\n")
  
(display (interpS (parse '#f)))
(display "\n")
  
(display (interpS (parse '(= 5 6))))
(display "\n")
  
(display (interpS (parse '(and (< 5 6) (> 10 9)))))
(display "\n")
  
(display (interpS (parse '(not (= 5 6)))))
(display "\n")
  
(display (parse '(not (= 5 6))))
(display "\n")

(display (parse '(- 5 6)))
(display "\n")

(display (desugar (parse '(- 5 6))))
(display "\n")
  
(display (parse '(~ 4)))
(display "\n")

