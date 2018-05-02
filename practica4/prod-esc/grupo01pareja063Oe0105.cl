;(defvar *ponderaciones* '((0.7711890452807111 0.7289637046682529 0.6822883072164541 0.5383486704522743 0.1128224464406089 0.10576143222823509)(0.9021471792429766 0.28560861517553393 0.3806319271304409 0.6976196666901252 0.9482484154311094 0.1669618134255314)))		0.91

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
           (ponderaciones (list '(0.7711890452807111 0.7289637046682529 0.6822883072164541 0.5383486704522743 0.1128224464406089 0.10576143222823509) '(0.9021471792429766 0.28560861517553393 0.3806319271304409 0.6976196666901252 0.9482484154311094 0.1669618134255314))))
           
           (+ (prod-esc-rec misfichas (first ponderaciones)) 
           	  (prod-esc-rec susfichas (second ponderaciones)))))

(defvar *alias* '|DaleMamasitaConTuTacata|) ; alias que aparecerá en el ranking
