;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Funciones adicionales para calcular cosine-distance-rec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Sumatorio de vectores (cuadrado)
(defun vct-sum(x)
	(if (null x)
		0
		(+(*(car x)(car x))(vct-sum(cdr x)))))

;;Producto cruzado
(defun cross-prod(x y)
	(cond
		((null x) 0)
		((null y) 0)
		(T (+ (* (car x) (car y))
			(cross-prod (cdr x) (cdr y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Funciones adicionales para calcular cosine-distance-mapcar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Sumatorio de vectores (cuadrado)
(defun vct-sum-mapcar(x)
	(apply #'+(mapcar #'(lambda (z) (* z z)) x)))

;;Producto cruzado
(defun cross-prod-mapcar (x y)
	(cond
		((null x) 0)
		((null y) 0)
		(T (apply #'+ (mapcar #'* x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-rec (x y)
	(if (or (= (vct-sum x) 0)
			(= (vct-sum y) 0))
		 NIL
		(- 1 (/ (cross-prod x y)
			(* (sqrt (vct-sum x))
				(sqrt (vct-sum y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-mapcar (x y)
  (if (or (equal (vct-sum-mapcar x) 0) (equal (vct-sum-mapcar y) 0))
		NIL
		(- 1 (/ (cross-prod-mapcar x y)
			(* (sqrt (vct-sum-mapcar x))
				(sqrt (vct-sum-mapcar y)))))))


;;CASOS DE PRUEBA:

;;(cosine-distance-rec '(1 2) '(1 2 3))
;;(cosine-distance-mapcar '(1 2) '(1 2 3))
;;(cosine-distance-rec nil '(1 2 3))
;;(cosine-distance-mapcar nil '(1 2 3))
;;(cosine-distance-rec '() '())
;;(cosine-distance-mapcar '() '())
;;(cosine-distance-rec '(0 0) '(0 0))
;;(cosine-distance-mapcar '(0 0) '(0 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence-level: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;
(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0))
  (if (or (<= (length lst-of-vectors) 1)
         (< (length vector) 1)
         (> confidence-level 1)
         (< confidence-level 0))
      NIL
	
			(mapcar #'rest (cosine-sim-category vector lst-of-vectors confidence-level))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones adicionales para calcular order-vectors-cosine-distance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cosine-sim-category (vector lista nivel)
	(if (null lista)
		'()
		(let* ((semejanza (- 1 (cosine-distance-rec vector (first lista))))
				(lista-recursiva (cosine-sim-category vector (rest lista) nivel)))
			(if (> semejanza nivel)
				(ins-ord-primero (cons semejanza (first lista)) lista-recursiva)
				lista-recursiva))))

(defun ins-ord-primero (elem lista)
	(if (null lista)
		(list elem)
		(if (> (first elem) (first(first lista)))
			(cons elem lista)
			(cons (first lista) (ins-ord-primero elem (rest lista))))))



;;CASOS DE PRUEBA:
;;(order-vectors-cosine-distance '(1 2 3) '((32 454 123) (133 12 1) (4 2 2)) 0.5)
;;(order-vectors-cosine-distance '(1 2 3) '((32 454 123) (133 12 1) (4 2 2)) 0.3)
;;(order-vectors-cosine-distance '(1 2 3) '((32 454 123) (133 12 1) (4 2 2)) 0.99)
;;(order-vectors-cosine-distance '(1 2 3) '())
;;(order-vectors-cosine-distance '() '((4 3 2) (1 2 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
(defun get-vectors-category (categories texts distance-measure)
	(if (or (null categories) (null texts))
		'()
		(let* ((lista-segunda (clasificar-mejor categories (rest texts) distance-measure))
				(lista-primera (clasificar-mejor categories texts distance-measure)))
			(if (null lista-segunda)
				(list (first lista-primera))
				(append (list (first lista-primera))
						(list (first lista-segunda)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones adicionales para calcular  get-vectors-category
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clasificar-mejor (categoria texto distancia)
	(if (or (null categoria) (null texto))
		'()
		(let* ((calculo-distancia (mapcar distancia (list(rest(first categoria))) 
										(list(rest(first texto))))))
			(ins-ord-segundo (cons (first(first categoria)) calculo-distancia) 
							(clasificar-mejor (rest categoria) texto distancia)))))

(defun ins-ord-segundo (elem lista)
	(if (null lista)
		(list elem)
		(if (< (second elem) (second (first lista)))
			(cons elem lista)
			(cons (first lista) (ins-ord-segundo elem (rest lista))))))

;; CASOS DE PRUEBA: 
;;(setf categories '((1 43 23 12) (2 33 54 24)))
;;(setf texts '((1 3 22 134) (2 43 26 58)))
;;(get-vectors-categories categories texts #'cosine-distance-rec)
;;(get-vectors-categories categories texts #'cosine-distance-mapcar)
;;(get-vectors-categories '(()) '(()) #'cosine-distance-mapcar)
;;(get-vectors-categories '((1 4 2) (2 1 2)) '((1 1 2 3)) #'cosine-distance-rec)
;;(get-vectors-categories '(()) '((1 1 2 3) (2 4 5 6)) #'cosine-distance-rec)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;         max-iter: maximo numero de iteraciones
;;;         x0: estimacion inicial del cero (semilla)
;;;         tol: tolerancia para convergencia (parametro opcional)
;;; OUTPUT: estimacion del cero de f o NIL si no converge
;;;

;;; x n+1 = xn - (f(n)/f'(n))

(defun newton (f df max-iter x0 &optional (tol 0.001))
	(let* ((ff (funcall f x0))
				(fprime (funcall df x0))
				(step (- x0 (/ ff fprime)))
				(dif (abs (- x0 step))))
				(cond
					((< dif tol) x0)
					((= max-iter 0) NIL)
					(t (newton f df (- max-iter 1)
							step
							tol)))))

;; CASOS DE PRUEBA
;; (newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 3.0)
;; (newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 0.6)
;; (newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 30 -2.5)
;; (newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 10 100.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f : funcion de la que se desea encontrar un cero
;;;        df : derivada de f
;;;        max-iter : maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: el primer cero de f que se encuentre , o NIL si se diverge
;;;          para todas las semillas
;;;

(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  	(if (null semillas) 
			nil
  	(let ((operacion (newton f df max-iter (first semillas) tol)))
	(if (null operacion)
		(one-root-newton f df max-iter (rest semillas) tol)
	operacion))))

;; CASOS DE PRUEBA:
;; (one-root-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(0.6 3.0 -2.5))
;; (one-root-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(3.0 -2.5))
;; (one-root-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 1 '(3.0 -2.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;

(defun all-roots-newton (f df max-iter semillas &optional ( tol 0.001))
  (if (null semillas)
  	nil
  	(append (list (newton f df max-iter (first semillas) tol)) 
			(all-roots-newton f df max-iter (rest semillas) tol))))

;; CASOS DE PRUEBA:
;; (all-roots-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(0.6 3.0 -2.5))
;; (all-roots-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(0.6 3.0 10000.0))
;; (all-roots-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 1 '(0.6 3.0 -2.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-not-nil-roots-newton
;;; Implementa una función haciendo uso de mapcan que pase 
;;; la salida de all-roots-newton a una lista de semillas (sin nil)
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;
(defun list-not-nil-roots-newton (f df max-iter semillas &optional (tol 0.001))
	(mapcan #'(lambda (x) 
					(if x (list x)))
					(all-roots-newton 
						f df max-iter semillas tol)))

;; CASOS DE PRUEBA:
;; (list-not-nil-roots-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3))) #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(0.6 3.0 10000.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst (elt lst)
	(if (or (equal elt nil) (equal lst nil))
		nil
		(mapcar #'(lambda (x) (list elt x)) lst))
  )

;; CASOS DE PRUEBA:
;; (combine-elt-lst 'a '(1 2 3))
;; (combine-elt-lst 'a nil)
;; (combine-elt-lst nil nil)
;; (combine-elt-lst nil '(a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas
(defun combine-lst-lst (lst1 lst2)
	(if (or (null lst1) (null lst2))
		nil
		(append (combine-elt-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2)))
  )

;; CASOS DE PRUEBA:
;; (combine-lst-lst '(a b c) '(1 2))
;; (combine-lst-lst nil nil)
;; (combine-lst-lst '(a b c) nil)
;; (combine-lst-lst nil '(a b c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos
(defun combine-list-of-lsts (lstolsts)
  (if (null (rest lstolsts))
  	(mapcar #'list (first lstolsts))
  	(if (null lstolsts)
  		nil
  		(mapcar #'(lambda (x) (apply #'cons x))
  			(nconc (combine-lst-lst (first lstolsts) (combine-list-of-lsts (rest lstolsts))))))))

;; CASOS DE PRUEBA:
;; (combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; (combine-list-of-lsts '(() (+ -) (1 2 3 4)))
;; (combine-list-of-lsts '((a b c) () (1 2 3 4)))
;; (combine-list-of-lsts '((a b c) (1 2 3 4) ()))
;; (combine-list-of-lsts '((1 2 3 4)))
;; (combine-list-of-lsts '(nil))
;; (combine-list-of-lsts nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defino operadores logicos
(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '!)

;; definiciones de valores de verdad, conectores y atomos
(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)
  (eql x +not+))

(defun and-p (x)
	(eql x +and+))

(defun or-p (x)
	(eql x +or+))

(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

(defun bicond-connector-p (x)
  (eql x +bicond+))

(defun cond-connector-p (x)
    (eql x +cond+))

(defun connector-p (x)
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))

(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

(defun negative-literal-p (x)
  (and (listp x)
       (eql +not+ (first x))
       (null (rest (rest x)))
       (positive-literal-p (second x))))

(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))

;;
(defun evaluar-cambio (lit)
	(cond
		((eq lit +and+) +or+)
		((eq lit +or+) +and+)
		(t lit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Funciones auxiliares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Traducir condicional
 (defun translate-cond (lst)
  (list +or+
        (list +not+ (first lst))
        (second lst)))

;;Traducir bicondicional
(defun translate-bicond (lst)
  (list +and+
        (translate-cond lst)
        (list +or+
              (cons +not+ (rest lst))
              (first lst))))

(defun expand (exp)
  (cond
    ((literal-p exp) (list exp))

    ((and-p (first exp))
     (expand (rest exp)))

    ((or-p (first exp))
     (mapcan #'expand (rest exp)))

    ((cond-connector-p (first exp))
     (expand (translate-cond (rest exp))))

    ((bicond-connector-p (first exp))
     (expand (translate-bicond (rest exp))))

    ((and (unary-connector-p (first exp)) 
		  			(listp (cadr exp)))
		  				(expand(list (negate (cadr exp)) (negate (rest (rest exp))))))

	;;((and (unary-connector-p (first exp)) (listp (second exp)))
	;;  		(expand (negate (rest exp))))
    (t exp)))

(defun evaluate (lst)
  (reduce #'(lambda (x y) (or x y))
          (mapcan #'(lambda (elt) (evaluate-aux elt lst)) lst)))

(defun evaluate-aux (elt lst)
  (mapcar #'(lambda (comp) (equal (negate elt) comp)) lst))

(defun negate (elt)
  (if (negative-literal-p elt)
      (second elt)
      (list +not+ elt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truth-tree
;;; Recibe una expresion y construye su arbol de verdad para
;;; determinar si es SAT o UNSAT
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : T   - FBF es SAT
;;;          N   - FBF es UNSAT
;;;
(defun truth-tree (exp)
  (unless (null exp)
    (not (evaluate (expand exp)))))


;; CASOS DE PRUEBA
;; (truth-tree '()) -> nil
;; (truth-tree '(^ (v A B))) -> t
;; (truth-tree '(^ (! B) B)) -> nil
;; (truth-tree '(<=> (=> (^ P Q) R) (=> P (v (! Q) R)))) -> t


;;Idea inicial
;;Traducir negacion (caso completo)
;;(defun negacion (lst)
;;	(unless (null lst)
;;	
;;	(cond 
;;		  	((binary-connector-p (first lst))
;;				(negacion (rest lst)))
;;
;;		   	((and-p (first lst))
;;		  		(cons +and+ (negacion (rest lst))))
;;
;;		  	((or-p (first lst))
;;		  		(cons +or+ (negacion (rest lst))))
;;		 
;;		  	((literal-p (first lst)) 
;;				(list (cons +not+ (first lst)) (negacion (rest lst))))
;;
;;		  	((and (unary-connector-p (first lst)) 
;;		  			(listp (cadr lst)))
;;		  				(append (negacion (cadr lst)) (negacion (rest (rest lst)))))
;;
;;		  	((listp (first lst))
;;		  		(append (negacion (first lst)) (negacion (rest lst)))))
;;
;;
;;	)
;;)


;;(defun sat (e) 
;;	(cond 
;;		((literal-p e) 
;;			(list (list e)))
;;		((eq 'V (car e))
;;			(append (sat (cadr e)) (sat (caddr e))))
;;
;;		(sat(rest e))))


;;(sat '(V A (! A)))

;; Alternativa: busco contradicciones
;;(defun unsat (e)
;;	(unless (or (null (car e))
;;				(null (cadr e)))
;;		(if (or (equal (cons +not+ (car e))
;;					   (cadr e))
;;				(unsat (rest e)))
;;			T
;;			(unsat (cons (car e) (cddr e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truth-tree
;;; Recibe una expresion y construye su arbol de verdad para
;;; determinar si es SAT o UNSAT
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : T   - FBF es SAT
;;;          N   - FBF es UNSAT
;;;

;;(defun truth-tree (fbf)
;;  (unless (null fbf)
;;  	
;;	  (if (unsat (expand-truth-tree-aux NIL fbf))
;;	  	NIL
;;	  	T)))

;; CASOS DE PRUEBA
;; (truth-tree '(=> A (^ B (! A))))
;; (truth-tree '(^ (=> (^ P I) L) (=> (¬ P) (¬ L)) (¬ P) L))

;;(defun expand-truth-tree-aux (base fbf)
;;	(if (null fbf)
;;		base
;;
;;	(cond 
;;		((cond-connector-p (first fbf))
;;			(expand-truth-tree-aux base (condicional (rest fbf))))
;;
;;	  	((bicond-connector-p (first fbf))
;;	  		(expand-truth-tree-aux base (bicondicional (rest fbf))))
;;
;;	  	((and-p (first fbf)) 
;;	  		(rest fbf))
;;
;;	  	((or-p (first fbf))
;;	  		(mapcan #'(lambda (x) 
;;	  			(expand-truth-tree-aux base (list x))) (rest fbf)))
;;	  
;;	  	((literal-p (first fbf))
;;	  		(expand-truth-tree-aux (cons (first fbf) base) (rest fbf)))
;;
;;	  	((and (unary-connector-p (first fbf)) (listp (second fbf)))
;;	  		(expand-truth-tree-aux base (rest fbf)))
;;
;;	  	((listp (first fbf))
;;	  		(expand-truth-tree-aux 
;;	  			(expand-truth-tree-aux base (first fbf)) 
;;	  			(rest fbf)))
;;		)
;;	)
;;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(expand-truth-tree-aux nil '(<=> (A B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path-improved
;;; Version de busqueda en anchura que no entra en recursion
;;; infinita cuando el grafo tiene ciclos
;;; INPUT:   end: nodo final
;;;          queue: cola de nodos por explorar
;;;          net: grafo
;;; OUTPUT: camino mas corto entre dos nodos
;;;         nil si no lo encuentra

(defun bfs-improved (end queue net)
  (if (null queue)
  	nil
  	(let((path (car queue)))
  		(let ((node (car path)))
  		(if (eql node end)
  			(reverse path)
  			(bfs-improved end
  				(append (cdr queue)
  					(new-paths-improved path node net))
  				net))))))

(defun new-paths-improved (path node net)
	(if (null (evaluar-repeticion path))
		nil
		(mapcar #'(lambda(x) (cons x path))
							(cdr (assoc node net)))))

(defun evaluar-repeticion (list)
	(or (null list)
		(and (not (member (first list) (rest list)))
			(evaluar-repeticion (rest list)))))

(defun shortest-path-improved (end queue net)
  (bfs-improved end queue net))

;; PRUEBAS
;; (shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))

