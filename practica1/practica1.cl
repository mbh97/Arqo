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
(defun sumatorio (x) (reduce #'+ x))

;;; Funcion que calcula el producto escalar de dos vectores
(defun prod-escalar (x y) (sumatorio (mapcar #'(lambda (z w) (* z w)) x y)))

;;; Funcion que calcula el modulo de un vector
(defun modulo (x) (sqrt (prod-escalar x x)))

;;; Funcion que calcula la longitud de una lista
(defun my-length (x) (if (null x) 0 (+ 1 (my-length (cdr x)))))

;;; Funcio que comprueba que todos los argumentos de una lista son mayores o iguales a 0
(defun lista-positiva (x) (not (some #'minusp x)))

;;; Funcion que comprueba si los argumentos pasados son correctos
(defun comprueba-arg (x y) (and (lista-positiva x) (lista-positiva y) (eql (my-length x) (my-length y))))

;;; Funcion sc-mapcar (x y)
(defun sc-mapcar (x y) (if (null (comprueba-arg x y)) nil (/ (prod-escalar x y) (* (modulo x) (modulo y)))))
