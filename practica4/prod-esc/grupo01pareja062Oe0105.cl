; (defvar *ponderaciones* '((0.9943945511339752 0.6263890230891831 0.5380591120147689 0.2153274661719341 0.19438387240613197 0.12717052751380753)(0.9093732518978621 0.23063666161064988 0.29291993181841924 0.7430791809741627 0.78060908695231 0.3579051175522805)))		0.91

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
           (ponderaciones (list '(0.9943945511339752 0.6263890230891831 0.5380591120147689 0.2153274661719341 0.19438387240613197 0.12717052751380753) '(0.9093732518978621 0.23063666161064988 0.29291993181841924 0.7430791809741627 0.78060908695231 0.3579051175522805))))
           
           (+ (prod-esc-rec misfichas (first ponderaciones)) 
           	  (prod-esc-rec susfichas (second ponderaciones)))))

(defvar *alias* '|MeRehusoADarteUnUltimoBeso|) ; alias que aparecerá en el ranking
