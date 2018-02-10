;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0...0) es NIL.
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: similitud coseno entre x e y

(defun no-vacia (lista)
	(not (eql nil lista)))

(defun no-cero (lista)
	(not (every #'zerop lista)))

(defun comprueba-arg (x y) 
	(and (no-vacia x) (no-vacia y) (no-cero x) (no-cero y)))

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

;;; Funcion sc-mapcar (x y)
(defun sc-mapcar (x y) 
	(if (null (comprueba-arg x y)) 
		nil 
		(/ (prod-escalar x y) (* (modulo x) (modulo y)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (cat vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; vs: vector de vectores
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud con respecto a la categoría es superior al
;;; nivel de confianza, ordenados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Funcion que elimina de lista de lista aquellos vectores cuya similitud sea menor al nivel de confianza
(defun limpia-lista (cat vs conf) 
	(remove-if #'(lambda (y) (< (abs (sc-rec cat y)) conf)) vs))


(defun sc-conf (cat vs conf) 
	(sort (limpia-lista cat vs conf) #'(lambda(y z) (> (sc-rec cat y) (sc-rec cat z)))))



;;Maria
(defun is-ok (cat vs conf) 
	(and (>= conf 0) (<= conf 1)))


;;; limpiar vs de longtudes distintas a x y de listas con elementos negativos

(defun limpia-lista1 (cat vs conf n) 
	(remove-if #'(lambda (y) (or (/= (my-length y) n) (not (es-positiva y)) (< (abs (sc-rec cat y)) conf))) vs))

(defun sc-conf1 (cat vs conf)
	(if (null (is-ok cat conf))
		nil
		(sort (limpia-lista1 cat vs conf (my-length cat)) #'(lambda(y z) (> (sc-rec cat y) (sc-rec cat z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;


(defun elimina-primero (lista)
  (rest lista))

(defun elimina-primero-lista (list)
  (mapcar #'(lambda (y) (elimina-primero y)) list))

(defun sc-classifier (cats texts func) 
	(mapcar #'(lambda (x) (append (list (first x) (funcall func (elimina-primero x) (first (sc-conf (elimina-primero x) (elimina-primero-lista texts) 0)))))) cats))

;; Probar con (sc-classifier '((1 2 3 4) (2 3 4 5)) '((1 3 4 5) (2 2 3 4)) #'sc-rec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finds a root of f between the points a and b using bisection.
;;
;; If f(a)f(b)>=0 there is no guarantee that there will be a root in the
;; interval, and the function will return NIL.
;; INPUT:
;; f: function of a single real parameter with real values whose root
;;
we want to find
;; a: lower extremum of the interval in which we search for the root
;; b: b>a upper extremum of the interval in which we search for the root
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;;
returns (a+b)/2 as a solution.
;; OUTPUT: Root of the function, or NIL if no root is found
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prod-funcion (f a b)
	(* (funcall f a) (funcall f b)))

(defun valor-medio (a b)
	(/ (- b a) 2))

(defun bisect (f a b tol)
	(if (or (>= (prod-funcion f a b) 0) (> a b))
		nil
		(if (<= (- b (valor-medio a b)) tol)
			(valor-medio a b)
			(if (<= (prod-funcion f b (valor-medio a b)) 0)
				(bisect f (valor-medio a b) b tol)
				(bisect f a (valor-medio a b) tol)))))

;(bisect #'(lambda (x) (sin (* 6.26 x))) 0.1 0.7 0.001)


























;;




