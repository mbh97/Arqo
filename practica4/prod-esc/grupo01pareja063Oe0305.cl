(defpackage :grupo01pareja063Oe0305 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo
(in-package grupo01pareja063Oe0305)

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
           (ponderaciones (list '(0.8928715474435598 0.5358699741860297 0.6731332210656937 0.3396544442275774 0.20360703092233368 0.0901646065785584) '(0.04712296330600674 0.9423527539160763 0.13211831958902487 0.9556273940383525 0.2607054612937526 0.19825079290045922))))
		   
		   ; Calcula el producto escalar
		   (+ (prod-esc-rec misfichas (first ponderaciones))
		   	  (prod-esc-rec susfichas (second ponderaciones)))))

(defvar *alias* '|__|) ; alias que aparecerá en el ranking
