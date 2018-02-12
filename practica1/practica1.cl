;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;                       EJERCICIO 1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones auxiliares utilizadas para sc-rec y sc-mapcar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; no-vacia (lista)
;;; Funcion que comprueba que un vector no es vacío
;;; INPUT: lista: vector, representado como una lista
;;; OUTPUT: T si la lista no es vacia, nil en caso contrario

(defun no-vacia (lista)
    (not (eql nil lista)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; no-cero (lista)
;;; Funcion que comprueba que una vector no es el vector cero
;;; INPUT: lista: vector, representado como una lista
;;; OUTPUT: t si la lista no es el vector 0, nil en caso contrario

(defun no-cero (lista)
    (not (every #'zerop lista)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; is-ok1
;;; Funcion que comprueba que los argumentos que recibe sc-rec son correctos (no son vectores vacíos ni son el vector cero)
;;; INPUT: x: primer vector a comprobar parametros
;;; y: segundo vector a comprobar parametros
;;; OUTPUT: t si los argumentos son correctos, nil en caso contrario

(defun is-ok1 (x y)  
    (and (no-vacia x) (no-vacia y) (no-cero x) (no-cero y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recursiva
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; prod-esc-rec (x y)
;;; Funcion que calcula el producto escalar de dos vectores de forma recursiva
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: producto escalar de los dos vectores

(defun prod-esc-rec (x y)  
    (if (null (rest x))  ; caso base
        (* (first x) (first y)) ; multiplica los primeros elementos de los vectores x y
        (+ (* (first x) (first y)) (prod-esc-rec (rest x) (rest y))))) ; sumatorio de todas estas multiplicaciones

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; modulo-rec (x)
;;; Función que calcula el módulo de un vector
;;; INPUT: x: vector, representado como una lista
;;; OUTPUT: raiz cuadrada del producto escalar de x x (definicion de modulo de x)

(defun modulo-rec (x)  
    (sqrt (prod-esc-rec x x))) ; raíz cuadrada del producto escalar de x x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0...0) es NIL.
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: similitud coseno entre x e y

(defun sc-rec (x y)  
    (if (null (is-ok1 x y))  ; condición de error
        nil  
        (/ (prod-esc-rec x y) (* (modulo-rec x) (modulo-rec y))))) ; aplica prod-esc-rec (x y)/(modulo-rec (x))*(modulo-rec (y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Ejemplos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (sc-rec nil '(1 2 3)) ;; --> nil
;;; (sc-rec '(0 0) '(7 3)) ;; --> nil
;;; (sc-rec '(1 2 3) '(4 1 7)) ;; --> 0.88823473

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iterativa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sumatorio (x)
;;; Funcion que calcula el sumatorio de los numeros de un vector
;;; INPUT: x: vector, representado como una lista
;;; OUTPUT: sumatorio de todos los elementos de x

(defun sumatorio (x) 
	(reduce #'+ x)) ; aplica la funcion suma a los elementos de x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; prod-escalar (x y)
;;; Funcion que calcula el producto escalar de dos vectores
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: sumatorio de la lista formada por el producto de los elementos de x e y

(defun prod-escalar (x y) 
	(sumatorio (mapcar #'(lambda (z w) (* z w)) x y))) ; sumatorio del producto de los elementos de dos vectores

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; modulo (x)
;;; Funcion que calcula el modulo de un vector
;;; INPUT: x, vector, representado como una lista
;;; OUTPUT: raiz cuadrada del producto escalar de x y x (definicion del modulo de x)

(defun modulo (x) 
	(sqrt (prod-escalar x x))) ; aplica la raiz cuadrada del producto escalar de x x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y

(defun sc-mapcar (x y) 
	(if (null (is-ok1 x y)) ; condicion de error
		nil 
		(/ (prod-escalar x y) (* (modulo x) (modulo y))))) ; aplica la definion de la similitud coseno

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Ejemplos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (sc-mapcar nil '(1 2 3)) ;; --> nil
;;; (sc-mapcar '(0 0) '(7 3)) ;; --> nil
;;; (sc-mapcar '(1 2 3) '(4 1 7)) ; --> 0.88823473

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          1.2 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; limpia-lista (cat vs conf)
;;; Funcion que elimina de lista de lista aquellos vectores cuya similitud sea menor al nivel de confianza
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; vs: vector de vectores
;;; conf: Nivel de confianza
;;; OUTPUT: nueva lista vs que no contiene aquellos elementos cuya similitud coseno con los vectores de cat sea menor que el nivel de confianza conf

(defun limpia-lista (cat vs conf) 
	(remove-if #'(lambda (y) (< (abs (sc-rec cat y)) conf)) vs)) ; elimina de vs aquellos vectores que al aplicarle similitud coseno con los vectores de vs sea menor que el nivel de confianza establecido

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; is-ok2 (cat vs conf)
;;; Funcion que comprueba que todos los vectores son distintos al vacio y al cero, y que el parametro conf se encuentra entre [0,1]
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; vs: vector de vectores
;;; conf: Nivel de confianza
;;; OUTPUT: t si los argumentos son correctos, nil en caso contrario

(defun is-ok2 (cat vs conf)
	(if (and (no-vacia cat) (no-cero cat) (>= conf 0) (<= conf 1)) ; si vs y conf son correctos
		(mapcar #'(lambda(x) (and (no-vacia x) (no-cero x))) vs) ; crea una lista con t o nil en funcion de si los vectores son correctos o no
		(list nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sc-conf (cat vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; vs: vector de vectores
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud con respecto a la categoría es superior al
;;; nivel de confianza, ordenados

(defun sc-conf (cat vs conf)
	(if (some #'null (is-ok2 cat vs conf))
		nil 
		(sort (limpia-lista cat vs conf) #'(lambda(y z) (> (sc-rec cat y) (sc-rec cat z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Ejemplos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (sc-conf '(1 43 23 12) '((1 3 22 134) nil) 0.2) ;; --> nil
;;; (sc-conf '(2 33 54 24) '((1 3 22 134) (1 3 22 134)) 0.2) ;; --> ((1 3 22 134) (1 3 22 134))
;;; (sc-conf '(1 2 3) '((4 2 1) (1 2 3)) 0.5) ;; --> ((1 2 3) (4 2 1))
;;; (sc-conf '(1 43 23 12) nil 0.2) ;; --> nil
;;; (sc-conf '(0 0 0 0) '((1 43 23 12)) 0.2) ;; --> nil
;;; (sc-conf '(1 2 3 4) '((1 43 23 12)) 4) ;; --> nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; is-ok3 (cats texts)
;;; Funcion que comprueba si los argumentos son correctos
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; OUTPUT: lista con los valores de verdad de los argumentos

(defun is-ok3 (cats texts)
	(if (or (null cats) (null texts))
		(list nil)
		(mapcar #'(lambda(x y) (and (no-vacia x) (no-cero x) (no-vacia y) (no-cero y))) cats texts))) ; comprueba que se cumpla todas las condiciones

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; encuentra-categoria (x cat f)
;;; Funcion que devuelve la categoria a la que pertenece un vector 
;;; INPUT: x: vector representado como una lista
;;; cat: vector de vectores, representao como una lista de listas
;;; f: funcion para evaluar la similitud coseno
;;; OUTPUT: par de identificador categoria e valor de la similitud coseno del vector con la categoria a la que pertenece

(defun encuentra-categoria (x cat f)
	(first (sort (mapcar #'(lambda (y) ; primer par de idenificador categoria tras haberlo ordenado
	(cons (first y) (funcall f (rest y) (rest x)))) cat) ; creacion de lista con identificador y similitud coseno 
	#'(lambda(x y) (> x y)) :key #'rest))) ; se ordena segun el segundo argumento


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorías.
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno

(defun sc-classifier (cats texts func)
	(if (some #'null (is-ok3 cats texts))
		nil
		(mapcar #'(lambda (x) (encuentra-categoria x cats func)) texts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Ejemplos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (setf cats '((1 43 23 12) (2 33 54 24)))
;;; (setf texts '((1 3 22 134) (2 43 26 58)))
;;; (sc-classifier cats texts #'sc-rec) ;; –-> ((2 . 0.48981872) (1 . 0.81555086))
;;; (sc-classifier cats texts #'sc-mapcar) ;; –-> ((2 . 0.48981872) (1 . 0.81555086))
;;; (sc-classifier nil texts #'sc-mapcar) ;; --> nil
;;; (sc-classifier '((0 0)) '((1 2)) #'sc-mapcar) ;; --> nil
;;; (sc-classifier '(nil) '((1 2)) #'sc-mapcar) ;; --> nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(time (sc-classifier cats texts #'sc-rec))
;; --> 
(time (sc-classifier cats texts #'sc-mapcar))
;; --> 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;                       EJERCICIO 2
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 RAÍCES DE UNA FUNCIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         2.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 Funciones auxiliares 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; prod-funcion (f a b)
;;; Calcula el producto de f(a) y f(b)
;;; INPUT: f: funcion a evaluar
;;;        a: numero en el que evaluar f
;;;        b: numero en el que evaluar f
;;; OUTPUT: producto de f(a) y f(b)

(defun prod-funcion (f a b)
	(* (funcall f a) (funcall f b)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; valor-medio (a b)
;;; Calcula el punto medio de (a b)
;;; INPUT: a: extremo inferior del intervalo 
;;;        b: extremo superior del intervalo
;;; OUTPUT: punto medio de (a b)   (a+b)/2

(defun valor-medio (a b)
	(/ (+ b a) 2))
				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bisect (f a b tol)
;; Finds a root of f between the points a and b using bisection.
;; INPUT:
;; f: function of a single real parameter with real values whose root
;;   we want to find
;; a: lower extremum of the interval in which we search for the root
;; b: b>a upper extremum of the interval in which we search for the root
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;;   returns (a+b)/2 as a solution.
;; OUTPUT: Root of the function, or NIL if no root is found				
				
(defun bisect (f a b tol)
	(if (or (>= (prod-funcion f a b) 0) (> a b))
		nil
		(if (< (- b a) tol)
			(valor-medio a b)
			(if (<= (prod-funcion f b (valor-medio a b)) 0)
				(bisect f (valor-medio a b) b tol)
				(bisect f a (valor-medio a b) tol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Ejemplos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(bisect #’(lambda(x) (sin (* 6.26 x))) 0.1 0.7 0.001)  ;;---> 0.5016602
;;;(bisect #’(lambda(x) (sin (* 6.26 x))) 0.0 0.7 0.001)  ;;---> NIL
;;;(bisect #’(lambda(x) (sin (* 6.28 x))) 1.1 1.5 0.001)  ;;---> NIL
;;;(bisect #’(lambda(x) (sin (* 6.28 x))) 1.1 2.1 0.001)  ;;---> NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;                                2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; allroot (f lst tol)
;; Finds all the roots that are located between consecutive values of a list
;; of values
;; INPUT:
;; f: function of a single real parameter with real values whose root we
;; 	want to find
;; lst: ordered list of real values (lst[i] < lst[i+1])
;; tol: tolerance for the stopping criterion: if b-a < tol the function 
;; 	returns (a+b)/2 as a solution.
;; OUTPUT: A list o real values containing the roots of the function in
;; the given sub-intervals

(defun allroot (f lst tol)
	(if (not (null (bisect f (first lst) (second lst) tol)))
		(if (null (third lst))
			(list(bisect f (first lst) (second lst) tol))
			(cons (bisect f (first lst) (second lst) tol) (allroot f (rest lst) tol)))
		(allroot f (rest lst) tol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           Ejemplos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (allroot #’(lambda(x) (sin (* 6.28 x))) ’(0.25 0.75 1.25 1.75 2.25) 0.0001)
;;;       ---> (0.50027466 1.0005188 1.5007629 2.001007)
;;; (allroot #’(lambda(x) (sin (* 6.28 x))) ’(0.25 0.9 0.75 1.25 1.75 2.25) 0.0001)
;;;       ---> (0.50027466 1.0005188 1.5007629 2.001007)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              2.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        FUNCIONES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; potencia (n)
;;; Calcula el resultado de elevar 2 a la potencia n
;;; INPUT: n: potencia a la que se eleva 2
;;; OUTPUT: 2 elevado a n

(defun potencia (n)
	(if (= n 0) 
		1
		(* 2 (potencia (- n 1)))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
;;; tam (a b i)
;;; Calcula el tamaño de los partes en que dividimos un intervalo
;;; INPUT: a: extremo inferior del intervalo
;;;        b: extremo suoerior del intervalo
;;;		   i: numero de subintervalos en los que hay que dividir (a b)
;;; OUTPUT: tamaño de los subintervalos
		
(defun tam (a b n)
	(/(- b a) (potencia n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; intervalo (a x tam)
;;; Calcula el siguiente extremo de los subintervalos
;;; INPUT: a: extremo inferior del intervalo original
;;;        x: numero de subintervalo a calcular
;;;        tam: tamaño que tiene que tener el subintervalo
;;; OUTPUT: extremo  del subintervalo

(defun intervalo (a x tam)
	(+ a (* x tam)))



(defun intervalo (a b n)
	(+ a (tam a b n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; lista (a b pot n)
;;; Agrupa en una lista los numeros que serán extremo de los intervalos
;;; en los que calcularemos la raiz de una funcion
;;; INPUT: a: extremo inferior del intervalo original
;;;        b: extremo superior del intervalo original
;;;        pot: numero de subintervalos en los que dividir (a b)
;;;        n: numero de intervalo que queremos calcular
;;; OUTPUT: extremo  del subintervalo

(defun lista (a b pot n) 
	(if (equal n pot)
		(list(intervalo a n (tam a b pot)))
		(cons (intervalo a n (tam a b pot)) (lista a b pot (+ n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;	allind (f a b N tol)
;;  Divides an interval up to a specified length and find all the roots of
;;  the function f in the intervals thus obtained.
;;  INPUT:
;;  f: function of a single real parameter with real values whose root
;;     we want to find
;;  a: lower extremum of the interval in which we search for the root
;;  b: b>a upper extremum of the interval in which we search for the root
;;  N: Exponent of the number of intervals in which [a,b] is to be divided:
;;     [a,b] is divided into 2^N intervals
;;  tol: tolerance for the stopping criterion: if b-a < tol the function
;;       returns (a+b)/2 as a solution.
;;  OUTPUT: List with all the found roots.
		
(defun allind (f a b N tol)
	(if (some #'null (lista a b (potencia N) 0))
	 nil
	(allroot f (lista a b (potencia N) 0) tol)))
	

(defun next-tam (N)
		(if(= N 1)
		0
		(log (-(potencia N) 1) 2)))
	
	
	
	
(defun allind (f a b N tol)
	(if(= N 0)
		(list(bisect f a b tol))
		(cons(bisect f a (intervalo a b n) tol) (allind f (intervalo a b n) b (next-tam N) tol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Ejemplos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (allind #’(lambda(x) (sin (* 6.28 x))) 0.1 2.25 1 0.0001)
;;;   --->  NIL
;;; (allind #’(lambda(x) (sin (* 6.28 x))) 0.1 2.25 2 0.0001)
;;;  ---> (0.50027084 1.0005027 1.5007347 2.0010324)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;                       EJERCICIO 3
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   COMBINACIÓN DE LISTAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           3.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; combine-elt-lst (elt lst)
;;; Combina un elemento dado con todos los elementos de una lista
;;; INPUT: elt: elemento que se combina con cada elemento de la lista
;;; 	   lst: lista de elementos a combinar
;;; OUTPUT:lista con el elemento combinado con cada elemento de la lista

(defun combine-elt-lst (elt lst)
		(if(null lst)
		nil
		(mapcar #'(lambda(x) (list elt x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           3.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; combine-lst-lst (lst1 lst2)
;;; Calcula el producto cartesiano de dos listas
;;; INPUT: lst1: primera lista
;;; 	   lst2: segunda lista
;;; OUTPUT: producto cartesiano de las listas

(defun combine-lst-lst (lst1 lst2)
        (if(or (null lst1) (null lst2))
            nil
            (mapcan #'(lambda (x) (combine-elt-lst x lst2))lst1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; combine-list-of-lsts (lstolsts) 
;;; Calcula todas las posibles disposiciones de elementos pertenecientes a N listas de 
;;; forma que en cada disposición aparezca únicamente un elemento de cada lista
;;; INPUT: lstolsts: lista de listas
;;; OUTPUT: lista con las posibles disposiciones. 

(defun combine-list-of-lsts (lstolsts) 
    (if (some #'null lstolsts)
        nil
        (if (null (rest lstolsts))
            (mapcar #'(lambda (x) (append (list x))) (first lstolsts)); caso base
            (mapcar #'(lambda (x) (cons (first x) (second x))) (combine-lst-lst (first lstolsts) (combine-list-of-lsts (rest lstolsts)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


