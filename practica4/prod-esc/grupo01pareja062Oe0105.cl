; 0.905

(defpackage :grupo01pareja062Oe0105 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo
(in-package grupo01pareja062Oe0105)


(defun list-fichas (tablero lado n)
	(let ((m (+ 1 n)))
	(if (>= n 5)
		(list (get-fichas tablero lado n))
	(cons (get-fichas tablero lado n) (list-fichas tablero lado m)))))

;;; Funcion que calcula el producto escalar de dos vectores de forma recursiva
(defun prod-esc-rec (x y)  
    (if (null (rest x))  ; caso base
        (* (first x) (first y)) ; multiplica los primeros elementos de los vectores x y
        (+ (* (first x) (first y)) (prod-esc-rec (rest x) (rest y))))) ; sumatorio de todas estas multiplicaciones

(defun heuristica (estado)
	(let* ((tablero (estado-tablero estado)) 
           (enMilado (estado-lado-sgte-jugador estado))
           (enLadoCont (lado-contrario (estado-lado-sgte-jugador estado)))
           (misfichas (list-fichas tablero enMilado 0))
           (susfichas (list-fichas tablero enLadoCont 0))
           (ponderaciones (list '(0.4306030449046129 0.08998642188462813 0.5666120644988062 0.009500610871207238 0.02198043754615009 0.008441687180968938) '(0.8799851730674326 0.9514165565949416 0.2092788014890138 0.11476630614388128 0.23057248623046867 0.01747075728892067))))
           
           (+ (prod-esc-rec misfichas (first ponderaciones)) 
           	  (prod-esc-rec misfichas (second ponderaciones)))))

(defvar *alias* '|MeRehusoADarteUnUltimoBeso|) ; alias que aparecerá en el ranking
