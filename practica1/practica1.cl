;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Funcion que calcula el sumatorio de una serie de numeros de una lista
(defun sumatorio (x) 
	(reduce #'+ x))

;;; Funcion que calcula el producto escalar de dos vectores
(defun prod-escalar (x y) 
	(sumatorio (mapcar #'(lambda (z w) (* z w)) x y)))

;;; Funcion que calcula el modulo de un vector
(defun modulo (x) 
	(sqrt (prod-escalar x x)))

;;; Funcion que calcula la longitud de una lista
(defun my-length (x) 
	(if (null x) 
		0 
		(+ 1 (my-length (cdr x)))))

;;; Funcio que comprueba que todos los argumentos de una lista son mayores o iguales a 0
(defun lista-positiva (x) 
	(not (some #'minusp x)))

;;; Funcion que comprueba si los argumentos pasados son correctos
(defun comprueba-arg (x y) 
	(and (lista-positiva x) (lista-positiva y) (eql (my-length x) (my-length y))))

;;; Funcion sc-mapcar (x y)
(defun sc-mapcar (x y) 
	(if (null (comprueba-arg x y)) 
		nil 
		(/ (prod-escalar x y) (* (modulo x) (modulo y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;

;;; Funcion que calcula el producto escalar recursivamente
(defun prod-esc-rec (x y) 
	(if (null (rest x)) 
		(* (first x) (first y))     ; null rest: multily the (only) element of the list
		(+ (* (first x) (first y)) (prod-esc-rec (rest x) (rest y)))))

;;; Funcion que calcula el modulo de un vector
(defun modulo-rec (x) 
	(sqrt (prod-esc-rec x x)))

;;; Funcion sc-rec (x y)
(defun sc-rec (x y) 
	(if (null (comprueba-arg x y)) 
		nil 
		(/ (prod-esc-rec x y) (* (modulo-rec x) (modulo-rec y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (x vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;

;;; Funcion que elimina de lista de lista aquellos vectores cuya similitud sea menor al nivel de confianza
(defun limpia-lista (x vs conf) 
	(remove-if #'(lambda (y) (< (abs (sc-rec x y)) conf)) vs))


(defun sc-conf (x vs conf) 
	(sort (limpia-lista x vs conf) #'(lambda(y z) (> (sc-rec x y) (sc-rec x z)))))



;;Maria
(defun is-ok (x conf) 
	(and (>= conf 0) (<= conf 1) (lista-positiva x)))

;;; limpiar vs de longtudes distintas a x y de listas con elementos negativos

(defun limpia-lista1 (x vs conf n) 
	(remove-if #'(lambda (y) (or (/= (my-length y) n) (not (lista-positiva y)) (< (abs (sc-rec x y)) conf))) vs))

(defun sc-conf1 (x vs conf)
	(if (null (is-ok x conf))
		nil
		(sort (limpia-lista1 x vs conf (my-length x)) #'(lambda(y z) (> (sc-rec x y) (sc-rec x z))))))









