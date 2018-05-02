;(defvar *ponderaciones* '((0.9068849390684273 0.8091881100994631 0.3541476987514376 0.4803992040461208 0.2449889028180624 0.0635658967708932)(0.04482298127677553 0.7331909827703357 0.7643537479250169 0.21368137761211758 0.5095619801962046 0.5817053154341711)))		0.92


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
           (ponderaciones (list '(0.9068849390684273 0.8091881100994631 0.3541476987514376 0.4803992040461208 0.2449889028180624 0.0635658967708932) '(0.04482298127677553 0.7331909827703357 0.7643537479250169 0.21368137761211758 0.5095619801962046 0.5817053154341711))))
           
           (+ (prod-esc-rec misfichas (first ponderaciones)) 
           	  (prod-esc-rec susfichas (second ponderaciones)))))

(defvar *alias* '|LlamadaDeEmergenciaBaby|) ; alias que aparecerá en el ranking
