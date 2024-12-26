#lang racket

; mapcar: recebe uma função e uma lista e retorna uma lista com f aplicado em cada elemento de l
(define mapcar (lambda (f l)
                 (if (null? l) `()
                     (cons (f (car l)) (mapcar f (cdr l))))))

(define +one (lambda (x) (+ 1 x)))

; curry: cria funções que fixam parcialmente parametros de outras funções -> ex: fixa x
(define curry (lambda (f)
                (lambda (x) (lambda (y) (f x y)))))

; mapc: fixa a função
(define mapc (curry mapcar))

; combine: aplica a função em todos elemtentos da lista e combina os resultados usando uma função binária (sum) e um zero
(define combine (lambda (f sum zero)
                  (lambda (l) (if (null? l) zero
                                  (sum (f (car l)) ((combine f sum zero) (cdr l)))))))

(define id (lambda (x) x))
;(combine id + 0) ;somatorio
;(combine id * 1) ;produtorio

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exercício 1

(define cdr* (lambda (l)
               (mapcar cdr l)))

;(cdr* '((a bc) (d e) (f)))


(define max* (lambda (l)
               ((combine id max 0) l)))

;(max* '(1 3 5 2 9 4))

(define append (lambda (l1 l2)
                 ((combine id cons l2) l1)))

;(append '(a b c) '(d e f))

(define addtoend (lambda (x l)
                   ((combine id cons (list x)) l)))

;(define addtoend(lambda (x l)
;                  (append l (list x))))

;(addtoend 'a '(b c d))

(define reverse (lambda (l)
                  (if (null? l) `()
                      (addtoend (car l) (reverse (cdr l))))))

;(reverse '(a b c d))


(define insert (lambda (x l)
                 (if (null? l) (list x)
                     (if (< x (car l)) (cons x l)
                                       (cons (car l) (insert x (cdr l)))))))


(define insertion-sort (lambda (l)
                         ((combine id insert '()) l)))

;(insertion-sort '(3 1 4 2 5))

(define mkpairfn (lambda (x)
                   (lambda (l) (mapcar (lambda (y) (cons x y)) l)))) ; lambda é o cons de (car l) = y com o x dado

;((mkpairfn 'a) '(() (b c) (d) ((e f))))

;;;;;;;;;;;;;;;;;;;;;;;;;
; exercício 2

; lex-order* é definida para receber uma funções de comparação (<)
; e retornar uma nova função que compara duas listas p1 e p2.

(define lex-order* (lambda (<)
                     (lambda (p1 p2)
                       (if (null? p1) #t
                         (if (null? p2) #f
                             (if (< (car p1) (car p2)) #t
                                 (if (< (car p2) (car p1)) #f
                                     ((lex-order* <) (cdr p1) (cdr p2)))))))))


(define alpha-order (lex-order* <))

;(alpha-order '(4 15 7) '(4 15 7)) 
;(alpha-order '(4 15 7) '(4 15 7 5)) 
;(alpha-order '(4 15 7) '(4 15 6 6)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exercício 3
(define nullset (lambda (x) #f))

(define member? (lambda (x s) (s x)))

; add element
; returning a new set that includes x as well elements in s
; uses the logical or to check if y is either a member of s or is equal to x.
(define addelt (lambda (x s)
                 (lambda (y)
                   (or (member? y s) (equal? x y)))))



(define set1 (addelt 1 (addelt 2 nullset))) ; {1, 2}
(define set2 (addelt 2 (addelt 3 nullset))) ; {2, 3}

;(member? 1 set1) ; returns #t
;(member? 3 set1) ; returns #f

(define union (lambda (s1 s2)
                (lambda (x)
                  (or (member? x s1) (member? x s2)))))


;((union set1 set2) 3)
;((union set1 set2) 5)

(define inter (lambda (s1 s2)
                (lambda (x)
                  (and (member? x s1) (member? x s2)))))

;((inter set1 set2) 2)
;((inter set1 set2) 3)


(define diff (lambda (s1 s2)
               (lambda (x)
                 (and (member? x s1) (not (member? x s2))))))

;((diff set2 set1) 3)
;((diff set2 set1) 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exercício 4

;(define gcd*-aux (lambda (l f)
;  (if (= (car l) 1) 1
;      (if (null? (cdr l)) (f (car l))
;          (gcd*-aux (cdr l)
 ;           (lambda (n) (f (gcd (car l) n))))))))

(define gcd*-aux (lambda (l f)
  (if (= (car l) 1) 1
      (if (null? (cdr l)) (f (car l))
                          (gcd*-aux (cdr l) (lambda (n) 
                                              (if (= (gcd (car l) n) 1) 1
                                                  (f (gcd (car l) n)))))))))

(define gcd* (lambda (l) (gcd*-aux l id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; lista 2

;remove todos os elementos daquela lista que satisfazem o preicado pred.
(define filtro (lambda (pred)
                 (lambda (l)
                   (if (null? l)`()
                       (if (pred (car l)) ((filtro pred) (cdr l))
                                          (cons (car l) ((filtro pred) (cdr l))))))))

;((filtro even?) '(1 2 3 4 5))


(define paratodos (lambda (pred l)
                    (if (null? l) #t
                        (if (pred (car l)) (paratodos pred (cdr l))
                                            #f))))

(define paratodos-combine (lambda (pred l)
                            ((combine pred (lambda (a b) (and a b)) #t) l)))


;(paratodos-combine even? '(1 2 6 4))
;(paratodos-combine even? '(2 4 6 8)) 

; find: dado um predicado pred acha se algum elemento de uma lista satisfaz a pred
(define find (lambda (pred lista)
               (if (null? lista) #f
                   (if (pred (car lista)) #t
                       (find pred (cdr lista))))))

(define paratodos-find (lambda (pred l)
                         (if (find (lambda (x) (not (pred x))) l) #f ; se algum pred não for satisfeito, todos não são pred
                                                                  #t))) 

;(paratodos-find even? '(2 4 6 8)) ; => #t
;(paratodos-find even? '(1 2 3 4 5)) ; => #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define combine (lambda (f sum zero)
                  (lambda (l) (if (null? l) zero
                                  (sum (f (car l)) ((combine f sum zero) (cdr l)))))))

(define combine-continuation (lambda (pred fun fbi zero)
                               (lambda (l) (combine-aux pred fun fbi zero l id))))

(define combine-aux (lambda (pred fun fbi zero l f)
                      (if (null? l) zero
                          (if (pred (car l)) zero
                              (if (null? (cdr l)) (f (fun (car l)))
                                                  (combine-aux pred fun fbi zero (cdr l) (lambda (x) (f (fbi (fun (car l)) x)))))))))

(define pow (lambda (x) (* x x)))

((combine-continuation even? pow + 0) `(1 9 5 7))

((combine-continuation even? pow + 0) `(1 3))
(define combine-continuation2 (lambda (pred fun fbi zero) (lambda (l) (call/cc (
                              lambda (exit) (letrec ((combine-aux2 (lambda (pred fun fbi zero l) (if (null? l)
                                                                (exit zero)
                                                                (if (pred (car l))
                                                                    (exit zero)
                                             (if (null? (cdr l)) (fun (car l))
                                             (fbi (fun (car l)) (combine-aux2 pred fun fbi zero (cdr l)))
                                                                    )))))) (combine-aux2 pred fun fbi zero l)))))))

((combine-continuation2 even? pow + 0) `(1 9 5 7))


(define mgcd* (lambda (l) (mgcd-aux* l id)))

(define mgcd-aux* (lambda (l f) (if (= (car l) 1) 1
                                    (if (null? (cdr l)) (f (car l))
                                        (mgcd-aux* (cdr l) (lambda (x) (f (gcd (car l) x))))))))




; Formula : seja uma funcao H com parametros H_1, H_2, ... H_n. Para usar fechamento basta:

;(define H (lambda (H_1 H_2 ... H_n l) (H-aux H_1 H_2, ... H_n l id)))
;(define H-aux (lambda (H_1 H_2 ... H_n l f) (if (..cond parada...) valParada
;                                                (if (null? (cdr l)) (f (car l))
;                                                    (H-aux H_1 H_2 ... H_n (cdr l) (lambda (x) (f (opbi (opun (car l)) x)))))))
; Onde x seria a chamada H-aux recursiva normalmente

(mgcd* `(2 4 2 4))



