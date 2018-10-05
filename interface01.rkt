#lang racket ;lenguaje a utilizar 
(require (lib "graphics.ss" "graphics"));Liberia de interface gráfica
(open-graphics);abre la libreria de graficos
(require racket/format);libreria para manejo de string
#|
+ Función: inicia el juego y manda a llamar la funcion que crea la ventana con el tablero o el error
+ Parámetros
  - M: número de columnas
  - N: número de filas
|#
(define (TTT M N)
  (cond
    ((and (or (> M 10) (< M 3))(or (> N 10) (< N 3))) (error 2 M N))
    ((or (> M 10) (< M 3)) (error 0 M 0))
    ((or (> N 10) (< N 3)) (error 1 0 N))
    (else (dibujoTablero (round (/ 700 M)) (round (/ 700 N)) 0 0 M N))
    )
  )
#|
+ Función: crea la ventana principal
|#
(define ventana (open-viewport "ventana" 700 700))
#|
+ Función: crea una ventana oculta, donde se dibuja todo
|#
(define oculta (open-pixmap "ventana" 700 700))
#|
+ Función: printea un error en pantalla en casi de estar incorrecto el número de filas o columnas
+ Parámetros:
  - tipo: marca que clase de error es (fila, columna o ambos)
  - col: número errado de columnas
  - fil: número errado de filas
|#
(define (error tipo col fil)
  (cond
    ((equal? tipo 0)((draw-string oculta)(make-posn 150 350) (~a col " no es un valor de columna permitido, debe de estar entre 3 y 10")))
    ((equal? tipo 1)((draw-string oculta)(make-posn 150 350) (~a fil " no es un valor de fila permitido, debe de estar entre 3 y 10")))
    (else
     (and
      ((draw-string oculta)(make-posn 150 200) (~a col " no es un valor de columna permitido, debe de estar entre 3 y 10\n"))
      ((draw-string oculta)(make-posn 150 500) (~a fil " no es un valor de fila permitido, debe de estar entre 3 y 10\n"))
      )
     )
    )
  (copy-viewport oculta ventana)
  )
#|
+ Función: crea las celdas del tablero en la ventana
+ Parámetros:
  - x: tamaño del ancho de la celda
  - y: tamaño del alto de la celda
  - col: contador de columnas 
  - fil: contador de filas
  - C: número de columnas
  - F: número de filas
|#
(define (dibujoTablero x y col fil C F)
  ((draw-solid-rectangle oculta)(make-posn (+ (* x col) 5) (+ (* y fil) 5)) (- x 10) (- y 10) "lightgray")
  (cond
    ((< col (- C 1)) (dibujoTablero x y (+ col 1) fil C F))
    (else
     (cond
       ((< fil (- F 1)) (dibujoTablero x y 0 (+ fil 1) C F))
       )
     )
    )
  (copy-viewport oculta ventana)
  )
(TTT 9 7)