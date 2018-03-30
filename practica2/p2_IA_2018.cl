;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Lab assignment 2: Search
;;    LAB GROUP: 2301
;;    Couple:  6
;;    Author 1: Maria Barroso 
;;    Author 2: Blanca Abella 
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-h                  ; reference to a function that evaluates to the 
                       ; value of the heuristic of a state
  f-goal-test          ; reference to a function that determines whether 
                       ; a state fulfils the goal 
  f-search-state-equal ; reference to a predictate that determines whether
                       ; two nodes are equal, in terms of their search state      
  operators)           ; list of operators (references to functions) to 
                       ; generate successors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *planets* '(Avalon Davion Katril Kentares Mallory Proserpina Sirtis))

(defparameter *white-holes*  
  '((Avalon Mallory 6.4) (Avalon Proserpina 8.6) 
  	(Mallory Proserpina 15) (Mallory Katril 10)
  	(Kentares Avalon 3) (Kentares Proserpina 7) (Kentares Katril 10)
  	(Davion Proserpina 5) (Davion Sirtis 6)
  	(Proserpina Avalon 8.6) (Proserpina Mallory 15) (Proserpina Davion 5) (Proserpina Sirtis 12)
  	(Katril Mallory 10) (Katril Davion 9)
  	(Sirtis Proserpina 12) (Sirtis Davion 6)))

(defparameter *worm-holes*  
  '((Avalon Kentares 4) (Avalon Mallory 9)
    (Davion Katril 5) (Davion Sirtis 8)  
    (Kentares Avalon 4) (Kentares Proserpina 12)
    (Mallory Avalon 9) (Mallory Katril 5)
    (Katril Mallory 5) (Katril Davion 5)
    (Katril Sirtis 10) (Sirtis Katril 10)
    (Sirtis Davion 8) (Sirtis Proserpina 9)
    (Proserpina Sirtis 9) (Proserpina kentares 12)
    (Proserpina Mallory 11) (Mallory Proserpina 11)))
 
(defparameter *sensors* 
  '((Avalon 15) (Mallory 12) 
  	(Kentares 14) (Davion 5) 
  	(Proserpina 7) (Katril 9) 
  	(Sirtis 0)))

(defparameter *planet-origin* 'Mallory)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden*   '(Avalon))
(defparameter *planets-mandatory*   '(Katril Proserpina))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristic
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state cost)
;;             where the first element is the name of a state and the second
;;             a number estimating the cost to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;
(defun f-h-galaxy (state sensors)
  (if (null sensors)
      nil
  (if (equal state (first (first sensors)))
      (second (first sensors))
  (f-h-galaxy state (rest sensors))))) 

(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth  *sensors*) ;-> NIL

;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;
;; Devuelve la lista de acciones que se pueden efectuar a partir de un estado 
;; a traves agujeros blancos
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    white-holes: Listas constantes de tripletes formados por planeta origen, 
;;     planeta destino y gasto energético (coste)
;; 
;;
;;  Returns: la lista de acciones que se pueden efectuar a partir de un estado 
;; a traves agujeros blancos
;;    

(defun get-actions (state holes planets-forbidden)
  (unless (null holes)
  (if (and (equal state (caar holes))(not (member (cadar holes) planets-forbidden :test #'equal))) ;;
        (cons (cdar holes) (get-actions state (rest holes) planets-forbidden))
        (get-actions state (rest holes) planets-forbidden))))


(defun navigate (state holes planets-forbidden name)  
  (mapcar #'(lambda(x) (make-action :name name
                                  :origin state
                                  :final (first x)
                                  :cost (second x))) (get-actions state holes planets-forbidden)))

(defun navigate-white-hole (state white-holes)
    (navigate state white-holes NIL 'NAVIGATE-WHITE-HOLE))

(defun navigate-worm-hole (state worm-holes planets-forbidden)
  (navigate state worm-holes planets-forbidden 'NAVIGATE-WORM-HOLE))


(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))

(navigate-worm-hole 'Mallory *worm-holes* NIL)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL AVALON :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))


(navigate-white-hole 'Kentares *white-holes*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL PROSERPINA :COST 7))


(navigate-worm-hole 'Uranus *worm-holes* *planets-forbidden*)  ;-> NIL


;;
;; END: Exercise 2 -- Navigation operators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3 -- Goal test
;;

(defun parents (node)
    (unless(null (node-parent node))
      (cons (node-state (node-parent node)) (parents (node-parent node)))))

(defun all-visited (parents planets-mandatory)
  (not(some #'null(mapcar #'(lambda(x) (member x parents :test #'equal)) planets-mandatory))))

(defun f-goal-test-galaxy (node planets-destination planets-mandatory) 
  (unless (not (member (node-state node) planets-destination))
    (all-visited (parents node) planets-mandatory)))


(defparameter node-01
   (make-node :state 'Avalon) )
(defparameter node-02
   (make-node :state 'Kentares :parent node-01))
(defparameter node-03
   (make-node :state 'Katril :parent node-02))
(defparameter node-04
   (make-node :state 'Kentares :parent node-03))
(f-goal-test-galaxy node-01 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-02 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-03 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-04 '(kentares urano) '(Avalon Katril)); -> T


;;
;; END: Exercise 3 -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3B  -- Equal predicate for search states
;;

(defun equal-lists (l1 l2)
	(and (null (set-difference l1 l2)) (null(set-difference l2 l1))))


(defun visited (node)
    (if (null (node-parent node))
    	(list (node-state node))
    (cons (node-state node) (visited (node-parent node)))))

(defun restantes (visited planets-mandatory)
	(remove-if #'(lambda(x) (member x visited :test #'equal)) planets-mandatory))


(defun f-search-state-equal-galaxy (node-1 node-2 &optional planets-mandatory)
  (if (equal (node-state node-1) (node-state node-2))
	(equal-lists (restantes (visited node-1) planets-mandatory) (restantes (visited node-2) planets-mandatory))
	nil))


       
(f-search-state-equal-galaxy node-01 node-01) ;-> T
(f-search-state-equal-galaxy node-01 node-02) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon)) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon Katril)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon Katril)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon Katril)) ;-> NIL


;;
;; END: Exercise  -- Equal predicate for search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 4 -- Define the galaxy structure
;;
;;
(defparameter *galaxy-M35* 
  (make-problem 
   :states               *planets*          
   :initial-state        *planet-origin*
   :f-h                  #'(lambda (state) (f-h-galaxy state *sensors*))
   :f-goal-test          #'(lambda (node) (f-goal-test-galaxy node *planets-destination* *planets-mandatory*))
   :f-search-state-equal #'(lambda (node-1 node-2) (f-search-state-equal-galaxy node-1 node-2 *planets-mandatory*))
   :operators            (list 
                          #'(lambda (state) (navigate-white-hole state *white-holes*))
                          #'(lambda (state) (navigate-worm-hole state *worm-holes* *planets-forbidden*)))))


;;
;;  END: Exercise 4 -- Define the galaxy structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 5: Expand node
;;


(defun union-actions (state problem)
	(append (funcall (first (problem-operators problem)) state) (funcall (second (problem-operators problem)) state)))




(defun expand-node (node problem)
	(mapcar #'(lambda(action) 
			(let ((gval (+ (node-g node) (action-cost action)))
				 (hval (funcall (problem-f-h problem) (action-final action))))
						(make-node :state (action-final action) 
									 :parent node 
									 :action action
									 :depth (+ (node-depth node) 1)
									 :g gval 
									 :h hval
									 :f (+ gval hval))))
	(union-actions (node-state node) problem)))





(defparameter node-00
   (make-node :state 'Proserpina :depth 12 :g 10 :f 20) )

(defparameter lst-nodes-00
  (expand-node node-00 *galaxy-M35*)) 


(print lst-nodes-00)

;;;(#S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13   :G 18.6  :H 15  :F 33.6)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13   :G 15    :H 5   :F 20)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)
;;;         :DEPTH 13   :G 25    :H 12  :F 37)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)
;;;         :DEPTH 13   :G 22    :H 0   :F 22)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)
;;;         :DEPTH 13   :G 22    :H 14  :F 36)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)
;;;         :DEPTH 13   :G 21    :H 12  :F 33)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)
;;;         :DEPTH 13   :G 19    :H 0   :F 19))



;;
;; END Exercise 5: Expand node
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 6 -- Node list management
;;;  
(defun insert-nodes-strategy (nodes lst-nodes strategy)
	(insert-nodes-strategy-aux nodes (insert-nodes-strategy-aux (rest lst-nodes) (list (first lst-nodes)) strategy) strategy))

(defun insert-nodes-strategy-aux (nodes lst-nodes strategy)
	(cond ((null nodes)
				lst-nodes)
		  ((null lst-nodes)
				(insert-nodes-strategy-aux (rest nodes) (list (first nodes)) strategy))
		  ((null (rest nodes))
		  		(if (funcall (strategy-node-compare-p strategy) (first nodes) (first lst-nodes))
		  			(cons (first nodes) lst-nodes)
		  			(cons (first lst-nodes) (insert-nodes-strategy-aux (list (first nodes)) (rest lst-nodes) strategy))))
		  ((if (funcall (strategy-node-compare-p strategy) (first nodes) (first lst-nodes))
		  			(insert-nodes-strategy-aux (rest nodes) (cons (first nodes) lst-nodes) strategy)
		  			(insert-nodes-strategy-aux (rest nodes) (cons (first lst-nodes) (insert-nodes-strategy-aux (list (first nodes)) (rest lst-nodes) strategy)) strategy)))))


(defparameter node-01
   (make-node :state 'Avalon :depth 0 :g 0 :f 0) )
(defparameter node-02
   (make-node :state 'Kentares :depth 2 :g 50 :f 50) )

(defun node-g-<= (node-1 node-2)
	(<= (node-g node-1)
	(node-g node-2)))

(defparameter *uniform-cost*
	(make-strategy
		:name 'uniform-cost
		:node-compare-p #'node-g-<=))

(defun node-g-<= (node-1 node-2)
	(<= (node-g node-1)
	(node-g node-2)))

(print (insert-nodes-strategy (list node-00 node-01 node-02) 
                        lst-nodes-00 
                        *uniform-cost*))

;;;(#S(NODE :STATE AVALON 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13    :G 18.6    :H 15    :F 33.6)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13    :G 15      :H 5     :F 20)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)
;;;         :DEPTH 13    :G 25      :H 12    :F 37)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)
;;;         :DEPTH 13    :G 22      :H 0     :F 22)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)
;;;         :DEPTH 13    :G 22      :H 14    :F 36)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)
;;;         :DEPTH 13    :G 21      :H 12    :F 33)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)
;;;         :DEPTH 13    :G 19      :H 0     :F 19)
;;; #S(NODE :STATE KENTARES 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 2 :G 50 :H 0 :F 50)) 


(print 
 (insert-nodes-strategy (list node-00 node-01 node-02) 
                        (sort (copy-list lst-nodes-00) #'<= :key #'node-g) 
                        *uniform-cost*));->
;;;(#S(NODE :STATE AVALON 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 0    :G 0     :H 0   :F 0)
;;; #S(NODE :STATE PROSERPINA 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 12   :G 10    :H 0   :F 20)
;;; #S(NODE :STATE DAVION
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 5)
;;;         :DEPTH 13   :G 15    :H 5   :F 20)
;;; #S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 8.6)
;;;         :DEPTH 13   :G 18.6  :H 15  :F 33.6)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 9)
;;;         :DEPTH 13   :G 19    :H 0   :F 19)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 11)
;;;         :DEPTH 13   :G 21    :H 12  :F 33)
;;; #S(NODE :STATE KENTARES
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 12)
;;;         :DEPTH 13   :G 22    :H 14  :F 36)
;;; #S(NODE :STATE SIRTIS
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 12)
;;;         :DEPTH 13   :G 22    :H 0   :F 22)
;;; #S(NODE :STATE MALLORY
;;;         :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 15)
;;;         :DEPTH 13   :G 25    :H 12  :F 37)
;;; #S(NODE :STATE KENTARES 
;;;         :PARENT NIL 
;;;         :ACTION NIL 
;;;         :DEPTH 2    :G 50    :H 0   :F 50))


(insert-nodes-strategy '(4 8 6 2) '(1 3 5 7)
		(make-strategy 	:name 'simple
					:node-compare-p #'<));-> (1 2 3 4 5 6 7 8)
 


;;
;;    END: Exercize 6 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 7 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;


(defun node-f-<= (node-1 node-2)
	(<= (node-f node-1)
	(node-f node-2)))

(defparameter *A-star*
  (make-strategy
		:name 'A-star
		:node-compare-p #'node-f-<=))

;;
;; END: Exercise 7 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 8: Search algorithm
;;;


(defun auxgs (node closed-nodes)
	(if (not(member node closed-nodes))
		nil
		(node-g-<= node (first (member node closed-nodes)))))


(defun graph-search-aux (problem strategy opened closed)
  (cond ;;((null opened) nil)		  
        ((not(null (f-goal-test-galaxy (first opened) *planets-destination* *planets-mandatory*))) 
		  	(first opened))
		  ((or(not (member (first opened) closed :test #'equal))(auxgs (first opened) closed))
		  	(and(insert-nodes-strategy(expand-node (first opened) problem) opened strategy)(cons (first opened) closed)(graph-search-aux problem strategy (rest opened) closed)))
	  	  (t(graph-search-aux problem strategy (rest opened) closed))))


(defun graph-search (problem strategy opened)
	(graph-search-aux problem strategy opened nil))

;  Solve a problem using the A* strategy
;
(defun a-star-search (problem)
		(graph-search problem *A-star*))


(graph-search *galaxy-M35* *A-star* (cons(make-node :state (problem-initial-state *galaxy-m35*)) nil));->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


(print (a-star-search *galaxy-M35*));->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


;;; 
;;;    END Exercise 8: Search algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 9: Solution path / action sequence
;;;
(defun solution-path (node)
  )

(solution-path nil) ;;; -> NIL 
(solution-path (a-star-search *galaxy-M35*))  ;;;-> (MALLORY ...)

(defun action-sequence-aux (node)
  ...)

(action-sequence (a-star-search *galaxy-M35*))
;;; ->
;;;(#S(ACTION :NAME ...)) 

;;; 
;;;    END Exercise 9: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 10: depth-first / breadth-first
;;;

(defparameter *depth-first*
  (make-strategy
   :name 'depth-first
   :node-compare-p #'depth-first-node-compare-p))

(defun depth-first-node-compare-p (node-1 node-2)
  ...)

(solution-path (graph-search *galaxy-M35* *depth-first*))
;;; -> (MALLORY ... )

(defparameter *breadth-first*
  (make-strategy
   :name 'breadth-first
   :node-compare-p #'breadth-first-node-compare-p))

(defun breadth-first-node-compare-p (node-1 node-2)
  ...)

(solution-path (graph-search *galaxy-M35* *breadth-first*))
;; -> (MALLORY ... )

;;; 
;;;    END Exercise 10: depth-first / breadth-first
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
