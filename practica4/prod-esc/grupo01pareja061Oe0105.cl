;(defvar *ponderaciones* '((0.8023491474847461 0.5932961348532672 0.27025098971087136 0.17356462784557136 0.36286798968493583 0.1558262968361046)(0.753406862321373 0.5970352233286638 0.5479114826101362 0.9817296740462751 0.4260328994146001 0.4006401272156591)))		0.87


(defpackage :grupo01pareja061Oe0105 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo
(in-package grupo01pareja061Oe0105)


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
           (ponderaciones (list '(0.8023491474847461 0.5932961348532672 0.27025098971087136 0.17356462784557136 0.36286798968493583 0.1558262968361046) '(0.753406862321373 0.5970352233286638 0.5479114826101362 0.9817296740462751 0.4260328994146001 0.4006401272156591))))
           
           (+ (prod-esc-rec misfichas (first ponderaciones)) 
           	  (prod-esc-rec susfichas (second ponderaciones)))))

(defvar *alias* '|LlamadaDeEmergenciaBaby|) ; alias que aparecerá en el ranking
