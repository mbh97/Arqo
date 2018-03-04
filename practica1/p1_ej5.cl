;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 					 5.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Grafo no dirigido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lista = ((1 2 3 5) (2 1 4 5) (3 1) (4 2) (5 1 2)) ESTA MAL
	1)	Visitado = 1
		Adyacente de 1 = 2,3,4
	2) 	Visitado = 1,2
		Adyacente de 2 = 1,4,5
	3) 	Visitado = 1,2,4
		Adyacente de 4 = 2
	3) 	Visitado = 1,2,4,5
		Adyacente de 5 = 2,1
	4) 	Visitado = 1,2,4,5,3
		Adyacente de 3 = 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 					 5.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

método DFS( origen):
	marcamos origen como visitado 
    	para cada vertice v adyacente a origen en el Grafo: 
			si v no ha sido visitado:
				marcamos como visitado v
                llamamos recursivamente DFS(v) 
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 			 		5.3-5.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
	(if (null queue) '() ;; camino vacio
		(let* ((path (first queue)) ;; path es la primera lista de adyacencia
			(node (first path))) ;; node es el primer elemento de path (vertice origen)
		(if (eql node end) ;; si hemos llegado al objetivo
			(reverse path) ;; devolvemos el path invertido
		(bfs end
			(append (rest queue) ;; nuevo camino con el resto de la lista de adyacencia
				(new-paths path node net)) ;; y la nueva lista de adyacencia
			net)))))
	
(defun new-paths (path node net)
	(mapcar #'(lambda(n)
		(cons n path)) ;; Crea lista de pares de cada asociacion de nodo-grafo con la primera lista de adyacencia
	(rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				5.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shortest-path (start end net)
	(bfs end (list (list start)) net))
	
	
;; (shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))





















