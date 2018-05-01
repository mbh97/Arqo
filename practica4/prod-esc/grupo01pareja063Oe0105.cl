;(defvar *ponderations* '((0.4725780288625071 0.7779916744680828 0.31716028169289223 0.20855088018545676 0.20707437747018698 0.09859977653244545)(0.39124711744576957 0.8988823405056647 0.973675398554397 0.6274984421059115 0.002517803623121928 0.27717334799285454)))		0.87

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
           (ponderaciones (list '(0.4725780288625071 0.7779916744680828 0.31716028169289223 0.20855088018545676 0.20707437747018698 0.09859977653244545) '(0.39124711744576957 0.8988823405056647 0.973675398554397 0.6274984421059115 0.002517803623121928 0.27717334799285454))))
           
           (+ (prod-esc-rec misfichas (first ponderaciones)) 
           	  (prod-esc-rec susfichas (second ponderaciones)))))

(defvar *alias* '|DaleMamasitaConTuTacata|) ; alias que aparecerá en el ranking
