#lang racket

(define (atom? x)
  (not (pair? x)))

(define (count x l)
  (if (null? l) 0
      (if (equal? x (car l)) (+ 1 (count x (cdr l)))
          (count x (cdr l)))))


(define (countall x l)
  (if (null? l) 0
      (if (atom? (car l))                  ; Se o primeiro elemento for um átomo
          (if (equal? x (car l))           ; Verifica se é igual a 'x'
              (+ 1 (countall x (cdr l)))   ; Conta e continua
              (countall x (cdr l)))        ; Continua sem contar
          (+ (countall x (car l)) (countall x (cdr l))))))      ; Se for uma sublista, conta dentro da sublista


;(count 'a '(1 b a (c a)))
;(countall 'a '(1 b a (c a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (reverse l)
  (if (null? l)`()
      (append (reverse (cdr l)) (list (car l)))))

;(reverse '(a b (c d) e))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (twist l)
  (if (null? l)`()
      (if (atom? (car l)) (append (twist (cdr l)) (list (car l)))               ; Reverte e adiciona o átomo à nova lista
                          (append (twist (cdr l)) (list (twist (car l)))))))    ; Reverte a sublista também

;(twist '((a (b 5)) (c d) e))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (flatten l)
  (if (null? l) '()
      (if (atom? (car l)) (cons (car l) (flatten (cdr l)))
                          (append (flatten (car l)) (flatten (cdr l))))))

;(flatten `((a b) ((c d) e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sublist l1 l2)
  (if (null? l1) #t
      (if (null? l2) #f
          (if (equal? (car l1) (car l2)) (sublist (cdr l1) (cdr l2))
                                         (sublist l1 (cdr l2))))))

;(sublist `(a b c) `(x a y b z c))


 (define (verifica l1 l2) 
   (if (null? l1) #t
       (if (null? l2) '()
           (if (equal? (car l1) (car l2)) (verifica (cdr l1) (cdr l2))
                                          '()))))


 (define (contig-sublist l1 l2)
   (if (null? l1) #t
      (if (null? l2) '()
          (if (equal? (car l1) (car l2)) (verifica (cdr l1) (cdr l2)) 
                                         (contig-sublist l1 (cdr l2))))))


;(contig-sublist '(a b c) '(x a y b z c))

;(contig-sublist '(a y) '(x a y b z c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exercício 2

(define (member? x s)
  (if (null? s) #f
      (if (equal? x (car s)) #t
                             (member? x (cdr s)))))

;(define nullset `())


(define (remove x s)
  (if (null? s) s
      (if (equal? x (car s)) (remove x (cdr s))
                             (cons (car s) (remove x (cdr s))))))

;(remove 'a '(a b c d))

(define (subset s1 s2)
  (if (null? s1) #t                                    
      (if (member? (car s1) s2) (subset (cdr s1) s2)       ; Se o primeiro elemento de s1 está em s2, verifica o restante da lista
                                #f)))

;(subset '(a b) '(a b c d))
;(subset '(a e) '(a b c d))

(define (=set s1 s2)
  (and (subset s1 s2) (subset s2 s1)))

;(=set '(a b c) '(c a b))
;(=set '(a b c) '(a b d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exercício 4

; post-order: esquerda - direita - nó
(define (post-order tree)
  (if (null? tree) '()
      (begin  
        (post-order (cadr tree))    ; segundo elemento - esquerda
        (post-order (caddr tree))   ; terceiro elemento - direita
        (display (car tree)))))     ; raiz

;(post-order '(A (B (D () ()) (E () ())) (C () ())))
;(D E B C A)

; in-order: esquerda - nó - direita
(define (in-order tree)
  (if (null? tree) '()
      (begin  
        (in-order (cadr tree))
        (display (car tree))
        (in-order (caddr tree)))))

;(in-order '(A (B (D () ()) (E () ())) (C () ()))) ;(D B E A C)

;pre-order: nó - esquerda - direita
(define (pre-order tree)
  (if (null? tree) '()
      (begin
        (display (car tree))
        (pre-order (cadr tree))
        (pre-order (caddr tree)))))

;(define (pre-order tree)
;  (if (atom? tree) (display tree)
;      (begin
;        (display (car tree))
;        (pre-order (cadr tree))
;        (pre-order (caddr tree)))))

;(pre-order '(A (B (D () ()) (E () ())) (C () ())))

; pre-order para grau arbitrario de arvore
(define (pre-order-arbitrary tree)
  (if (atom? tree) tree
      (append (list (car tree)) (pre-order-arbitrary (cdr tree)))))
              

;(pre-order-arbitrary '(a (b e f) c (d g)))








