#lang scheme

(require racket/stream)

; 1.
; a) (itera f x) que retorna a ista inXinita (x, f(x), f(f(x))....)

(define itera
  (lambda (f x)
    (stream-cons x (itera f (f x)))))

(stream->list (stream-take (itera (lambda (x) (+ 1 x)) 1) 7))

; b) (ciclo l) que recebe como parâmetro uma lista finita l e retorna a repetição infinita de l.

(define ciclo
  (lambda (l)
    (letrec ((cicle_list (lambda () (stream-append (stream* l) (stream-lazy(cicle_list))))))
      (cicle_list))))

(stream->list (stream-take (ciclo '(1 2 3 4)) 16))

; c) (fib) que retorna a lista de todos os números de Fibonacci,
;    isto é, a lista (1 1 2 3 5 8...).

(define stream-map2
  (lambda (f l1 l2)
    (stream-cons (f (stream-first l1) (stream-first l2))
          (stream-map2 f (stream-rest l1) (stream-rest l2)))))

(define fib
  (stream-cons 1(stream-cons 1(stream-map2 + fib (stream-rest fib)))))

(stream->list (stream-take fib 12))

;    Em seus comentários, dê a fórmula geral para esquemas de recursão do tipo
;    x0 = <valor inicial0>
;    x1 = <valor inicial1>
;    x2 = <valor inicial2>
;    xi+3 = f(xi+2 ,xi+1 ,xi , i+3) que podemos reescrever como
;    xi = f(xi-1, xi-2, xi-3, i), para i começando em 3

; (define inteiros (stream-cons 1 (stream-map (lambda (x) (+ x 1)) inteiros)))
  
; (define esquema
;   (stream-cons x0 (stream-cons x1 (stream-cons x2 (stream-map4 + esquema (stream-rest esquema)
;                                                             (stream-rest(stream-rest esquema)) (stream-rest(stream-rest inteiros)))))))


; 2. Escreva a função (merge l1 l2) que recebe duas listas infinitas
; (x1 ....) (y1 ...) e as junta numa única lista (x1 ,y1,x2 ,y2,.....)

(define merge
  (lambda (l1 l2)
    (stream-cons (stream-first l1)
                 (stream-cons (stream-first l2) (merge (stream-rest l1) (stream-rest l2))))))

(define l1 (itera (lambda (n) (+ n 2)) 1)) ; generates (1, 3, 5, 7, 9, 11...)
(define l2 (itera (lambda (n) (+ n 3)) 2)) ; generates (2, 5, 8, 11, 14, 17 ...)
(stream->list (stream-take (merge l1 l2) 10))

; 3.

(define foreach-inf (lambda (lista f)
                  (if (null? lista) '()
                      (stream-cons (f (stream-first (stream-first lista)))
                                   (foreach-inf (stream-append (stream-rest lista) (list (stream-rest (stream-first lista)))) f)))))


(define square (lambda (x) (* x x)))
(define inteirosde2 (itera (lambda (n) (+ n 1)) 2)) ; (2, 3, 4, 5, ...)
(define inteirosde10 (itera (lambda (n) (+ n 1)) 10)) ; (10, 11, 12, ...)
(stream->list (stream-take (foreach-inf (list inteirosde2 inteirosde10) square) 4))
                  




                                           

