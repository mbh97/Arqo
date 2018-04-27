(defpackage :grupo01pareja061Oe2604 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo
(in-package grupo01pareja061Oe2604)


(defun numVacias-aux (tablero lado n)
  (let* ((m (+ 1 n)))
  (cond ((>= n 6) 0)
      ((eq 0 (get-fichas tablero lado n))
          (+ (numVacias-aux tablero lado m) 1))
      (t(numVacias-aux tablero lado m)))))

(defun numVacias (tablero lado)
  (numVacias-aux tablero lado 0))

; Necesito saber mis fichas, numero de casillas vacias, puntos en kalaha 
(defun heuristica (estado)
  (let* ((tablero (estado-tablero estado)) 
           (enMilado (estado-lado-sgte-jugador estado))
           (misFichas (cuenta-fichas tablero enMilado 0))
           (misVacias (numVacias tablero enMilado))
           (miKalaha (get-fichas tablero enMilado 6)))
     (+ (* misFichas 0.6)(* misVacias 0.4) (* miKalaha 0.7))))

(defvar *alias* '|ConTuFiguraQueMeAtrapaTrapa|) ; alias que aparecerá en el ranking

