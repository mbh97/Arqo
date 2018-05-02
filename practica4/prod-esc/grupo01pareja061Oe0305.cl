(defpackage :grupo01pareja061Oe0305 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo
(in-package grupo01pareja061Oe0305)

;;; Funcion que crea una lista con el numero de fichas que hay en un lado del tablero
(defun list-fichas (tablero lado n)
	(let ((m (+ 1 n)))
	(if (>= n 5)
		(list (get-fichas tablero lado n))
	(cons (get-fichas tablero lado n) (list-fichas tablero lado m)))))

;;; Funcion que calcula el producto escalar de dos vectores de forma recursiva (recogida de la practica 1)
(defun prod-esc-rec (x y)  
    (if (null (rest x))  ; caso base
        (* (first x) (first y)) ; multiplica los primeros elementos de los vectores x y
        (+ (* (first x) (first y)) (prod-esc-rec (rest x) (rest y))))) ; sumatorio de todas estas multiplicaciones


;;; Funcion que hace el producto escalar del vector con el numero de fichas de cada lado y un vector de ponderaciones, cuyo origen esta explicado en la memoria
(defun heuristica (estado)
	(let* ((tablero (estado-tablero estado)) 
           (enMilado (estado-lado-sgte-jugador estado))
           (enLadoCont (lado-contrario (estado-lado-sgte-jugador estado)))
           (misfichas (list-fichas tablero enMilado 0))
           (susfichas (list-fichas tablero enLadoCont 0))
           (ponderaciones (list '(0.9266943854845165 0.5824572973590543 0.0076888826706502655 0.1145102517741492 0.048694063180721114 0.0023492004526195487) '(0.01638638959138894 0.6990173710366768 0.688741690096616 0.7631014211693712 0.7226444933268606 0.010315793627031167))))
		   
		   ; Calcula el producto escalar
		   (+ (prod-esc-rec misfichas (first ponderaciones))
		   	  (prod-esc-rec susfichas (second ponderaciones)))))

(defvar *alias* '|__|) ; alias que aparecerá en el ranking
