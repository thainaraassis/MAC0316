

(test ; Métodos e variáveis de instância funcionam
  (interpS '(let Classezinha (class Object i
                         (regularMethod beep x x)
                         (regularMethod boop _ i))

            (let objetinho (new Classezinha 1)
              (+ (send objetinho beep 10)
                 (send objetinho boop 0)
                 ))))
  (numV 11))

(test ; Self funciona
  (interpS '(let Classezinha (class Object i 
                          (regularMethod beep x (+ 10 x))
                          (regularMethod boop x (send self beep (+ 1 x))))

              (let objetinho (new Classezinha 0) 
                (send objetinho boop 0))))
  (numV 11))

(test ; Sobrescrita funciona
  (interpS '(let Classezinha1 (class Object i 
                          (regularMethod beep _ 9)
                          (regularMethod boop _ 10))

              (let Classezinha2 (class Classezinha1 j
                            (regularMethod beep _ 1)
                            (regularMethod unused _ 0))

                (let objetinho (new Classezinha2 0) 
                  (+ (send objetinho beep 0)
                     (send objetinho boop 0))))))
  (numV 11))

(test ; Sobrescrita com self funciona
  (interpS '(let Classezinha1 (class Object i 
                          (regularMethod beep x 9)
                          (regularMethod boop x (send self beep x)))

              (let Classezinha2 (class Classezinha1 j
                            (regularMethod beep _ 11)
                            (regularMethod unused _ 0))

                (let objetinho (new Classezinha2 0) 
                  (send objetinho boop 0)))))
  (numV 11))

(test ; Set com variáveis de instância funciona
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod setVariable x (set! i x))
                           (regularMethod getVariable x i))

              (let objetinho (new Classezinha1 9)
                (seq (send objetinho setVariable 11)
                     (send objetinho getVariable 0)))))
  (numV 11))


(test ; Variáveis de instância classes filhas funcionam corretamente
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod beep x x))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod getJ _ j)
                             (regularMethod beep x x))

                (let objetinho (new Classezinha2 11)
                  (send objetinho getJ 0)))))
  (numV 11))

(test ; Acessar de instância de classes ancestrais devolve null
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod beep x x))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod getJ _ j)
                             (regularMethod beep x x))

                (let objetinho (new Classezinha2 9)
                  (send objetinho getI 0)))))
  (nullV))


(test ; É possível acessar e modificar variáveis de instância dos pais
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod setI x (set! i x)))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod getJ _ j)
                             (regularMethod setJ x (set! j x)))

                (let Classezinha3 (class Classezinha2 
                                    k 
                                    (regularMethod getK _ k)
                                    (regularMethod setK x (set! k x))
                                    )

                (let objetinho (new Classezinha3 9)
                  (seq 
                    (send objetinho setI 1)

                    (seq 
                       (send objetinho setJ 10)

                       (seq 
                         (send objetinho setK 100)

                         (+ (send objetinho getI 0)
                            (+ 
                              (send objetinho getJ 0)
                              (send objetinho getK 0)))
                         ))))))))
  (numV 111))

(test ; É possível modificar variáveis de instância dos pais pelos métodos dos filhos
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod unused _ 0))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod setI x (set! i x))
                             (regularMethod getJ _ j))

                (let Classezinha3 (class Classezinha2 
                                    k 
                                    (regularMethod setJ x (set! j x))
                                    (regularMethod unused _ 0))

                (let objetinho (new Classezinha3 9)
                  (seq 
                    (send objetinho setI 1)

                    (seq 
                       (send objetinho setJ 10)

                       (+ 
                         (send objetinho getI 0)
                         (send objetinho getJ 0))
                       )))))))
  (numV 11))

(test ; Objetos são independentes
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod setI x (set! i x)))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod getJ _ j)
                             (regularMethod setJ x (set! j x)))

                (let objetinho1 (new Classezinha2 1)
                  (seq 
                    (send objetinho1 setI 10)

                    (let objetinho2 (new Classezinha2 100) 
                      (seq 
                        (send objetinho2 setI 1000)

                        (+ (send objetinho1 getI 0)
                           (+ 
                             (send objetinho1 getJ 0)
                             (+ (send objetinho2 getI 0)
                                (send objetinho2 getJ 0))))
                        )))))))
  (numV 1111))
