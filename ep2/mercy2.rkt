
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
