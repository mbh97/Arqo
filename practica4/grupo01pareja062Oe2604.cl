
(defpackage :grupo01pareja062Oe2604 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo
(in-package grupo01pareja062Oe2604)

(defun heuristica (estado) ; función de evaluación heurística a implementar
  (let ((tablero (estado-tablero estado)) 
        (enMilado (estado-lado-sgte-jugador estado))
        (enLadoCont (lado-contrario (estado-lado-sgte-jugador estado))))
  (-(*(get-fichas tablero enMilado 6) (get-pts enMilado)) ; puntos en mi lado 
    (*(get-fichas tablero enLadoCont 6) (get-pts enLadoCont))))) ; puntos en lado contrario


(defvar *alias* '|DiseloYandel|) ; alias que aparecerá en el ranking
