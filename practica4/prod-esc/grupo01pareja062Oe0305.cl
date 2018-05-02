(defpackage :grupo01pareja062Oe0305 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo
(in-package grupo01pareja062Oe0305)

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
           (ponderaciones (list '(0.9598683086291033 0.945947750210964 0.4938095142195834 0.15311872339160082 0.27894724392316494 0.2072734978836971) '(0.001730720076191239 0.6570074200210185 0.8250205990648752 0.9421297920225978 0.6372135288615126 0.13900940946694873))))
		   
		   ; Calcula el producto escalar
		   (+ (prod-esc-rec misfichas (first ponderaciones))
		   	  (prod-esc-rec susfichas (second ponderaciones)))))

(defvar *alias* '|__|) ; alias que aparecerá en el ranking
