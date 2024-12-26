#lang scheme
(require racket/stream)


(define ints-from (lambda (i) (stream-cons i (ints-from (+ 1 i)))))
(define ints (ints-from 0))
(define posints (ints-from 1))

(define mapcar (lambda (f l)
                 (if (null? l) '()
                     (stream-cons (f (stream-first l)) (mapcar f (stream-rest l))))))

(define mapcar2 (lambda (f l1 l2)
                  (if (empty? l1)'()
                      (if (empty? l2) '()
                          (stream-cons (f (stream-first l1) (stream-first l2)) (mapcar2 f (stream-rest l1) (stream-rest l2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define evens
  (stream-cons 0 (mapcar (lambda (x) (+ x 2)) evens)))

  
;(stream->list (stream-take evens 10))


(define next (lambda (n xi) (/ (+ xi (/ n xi)) 2)))

(define xlist (lambda (n)
                (stream-cons 1 (mapcar (lambda (x) (next n x)) (xlist n)))))

;(stream->list (stream-take (xlist 100) 11))

(define binoms (stream-cons '(1)
                            (mapcar (lambda (ant) (stream->list (stream-append `(1) (mapcar2 + ant (stream-rest ant))
                                                                               `(1))))
                                    binoms)))
  
;(stream->list (stream-take binoms 5))

(define fibonacci (stream-cons 0 (stream-cons 1 (mapcar2 (lambda (x y) (+ x y)) fibonacci (stream-rest fibonacci)))))

;(stream->list (stream-take fibonacci 10))


