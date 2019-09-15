(use-package 'conecta4)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; -------------------------------------------------------------------------------
;; Funciones de evaluación 
;; -------------------------------------------------------------------------------

(defun f-eval-bueno (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual))) 
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000)))))
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-oponente columna fila))
		     (der (contar-derecha tablero ficha-oponente columna fila))
		     (izq (contar-izquierda tablero ficha-oponente columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
		(setf puntuacion-oponente
		      (+ puntuacion-oponente
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000))))))
	(- puntuacion-actual puntuacion-oponente)))))


;; -------------------------------------------------------------------------------
;; Pruebas Dia 11 Abril
;; -------------------------------------------------------------------------------

(defun prueba1 (estado)
	(if (juego-terminado-p estado)
		+val-max+
		+val-min+))

(defun prueba2 (estado)
	(if (generar-sucesores estado) ; no esta bien, de esto se encarga minimax
		0
		1))

(defun prueba3 (estado)
	(random estado))


(defun prueba5 (estado)
	(let* ((tablero (estado-tablero estado))
		(ficha-actual (estado-turno estado))
		(ficha-oponente (siguiente-jugador ficha-actual))
		(puntuacion-actual 0)
		(puntuacion-oponente 0))
		(loop for columna from 0 below (tablero-ancho tablero) do
			(let* ((altura (altura-columna tablero columna))
				(fila (1- altura))
				(num-fichas (contar-abajo tablero ficha-oponente columna fila)))
				(if (= num-fichas 3)
					(setf  puntuacion-oponente (+ puntuacion-oponente 100))
					(setf puntuacion-actual (+ puntuacion-actual 50)))))
		(- puntuacion-actual puntuacion-oponente)))

(defun prueba10 (estado)
	(let* ((tablero (estado-tablero estado))
		(sucesores (generar-sucesores estado))
		(mejor-suc (first sucesores)))
		(loop for columna from 0 below (tablero-ancho tablero) do
			(let* ((altura (altura-columna tablero columna))
					(fila (1- altura)))
			(loop for sucesor in sucesores do
				(setf val (val (negamax-1 (estado *my-random-state* (1- altura) copiar-estado))))
				(when (< val +val-max+)
					(setf mejor-val val)
					(setf mejor-suc fila)))
			(setf (val mejor-suc) mejor-val) mejor-suc))))

(defun prueba4 (estado)
	(let* ((tablero (estado-tablero estado))
		(ficha-actual (estado-turno estado))
		(ficha-oponente (siguiente-jugador ficha-actual))
		(puntuacion-actual 0)
		(puntuacion-oponente 0))
		(loop for columna from 0 below (tablero-ancho tablero) do
			(let* ((altura (altura-columna tablero columna))
				(fila (1- altura))
				(num-fichas (contar-abajo tablero ficha-oponente columna fila)))
				(setf  puntuacion-oponente 
					(+ puntuacion-oponente 
					(cond ((= num-fichas 0) 0)
						((= num-fichas 1) 5)
						((= num-fichas 2) 10)
						((= num-fichas 3) 15))))
				(setf  puntuacion-actual 
					(+ puntuacion-actual 
					(cond ((= num-fichas 0) 15)
						((= num-fichas 1) 10)
						((= num-fichas 2) 5)
						((= num-fichas 3) 0))))))
		(- puntuacion-actual puntuacion-oponente)))

(defun conAltura (estado)
	(let* ((tablero (estado-tablero estado))
			(ficha-actual (estado-turno estado))
			(ficha-oponente (siguiente-jugador ficha-actual))
			(acciones (acciones-posibles estado))
			(puntuacion-asc 0)
			(puntuacion-desc 0)
			(puntuacion-abajo 0)
			(puntuacion-arriba 0))
		(loop for columna from 0 below (tablero-ancho tablero) do
			(let* ((altura (altura-columna tablero columna))
				(fila (1- altura))
				(num-fichas-abajo (contar-abajo tablero ficha-oponente columna fila))
				(num-fichas-arriba (contar-arriba tablero ficha-oponente columna fila))

				(num-fichas-diagonal-asc (+ (contar-abajo-izquierda tablero ficha-oponente columna fila)
											(contar-arriba-derecha tablero ficha-oponente (1+ columna) (1+ fila))))
				(num-fichas-diagonal-desc (+ (contar-abajo-derecha tablero ficha-oponente columna fila)
											 (contar-arriba-izquierda tablero ficha-oponente (1- columna) (1+ fila)))))
				(cond ((null acciones) 0)
					(t (setf  puntuacion-asc 
						(+ puntuacion-asc 
						(cond ((= num-fichas-diagonal-asc 0) 0)
							((= num-fichas-diagonal-asc 1) 100)
							((= num-fichas-diagonal-asc 2) 200)
							((= num-fichas-diagonal-asc 3) 3000)
							(t (+ puntuacion-asc 10000)))))
					(setf  puntuacion-abajo 
						(+ puntuacion-abajo 
						(cond ((= num-fichas-abajo 0) 0)
							((= num-fichas-abajo 1) 100)
							((= num-fichas-abajo 2) 1000)
							((= num-fichas-abajo 3) 10000)
							(t (+ puntuacion-abajo 100000)))))
					(setf  puntuacion-arriba 
						(+ puntuacion-arriba 
						(cond
							((= num-fichas-abajo 3) 1000)
							((= num-fichas-abajo 4) 10000)
							(t 0))))
					(setf  puntuacion-desc 
						(+ puntuacion-desc 
						(cond ((= num-fichas-diagonal-desc 0) 3000)
							((= num-fichas-diagonal-desc 1) 200)
							((= num-fichas-diagonal-desc 2) 100)
							((= num-fichas-diagonal-desc 3) 0)
							(t (+ puntuacion-desc 10000)))))))
			
			))

		(+ (* (- 0 1) (- puntuacion-desc puntuacion-asc)) (+ puntuacion-abajo puntuacion-arriba))

	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Busca el maximo de casillas consecutivas
(defun consecutivas(lista)
	(let ((maxval 0) (val 0))
	(loop for x in lista do
		(cond ((not (null x))
			(setf val (1+ val))
			(if (< maxval val)
				(setf maxval val)
				nil))
		(t (setf val 0))))
	maxval))

;;Busca la columna mas central
(defun busca-centro(columna)
	(if (< columna (/ 7 2))
		(mod columna (/ 7 2))
		(- 7 columna)))

;;Cuenta el numero de fichas
(defun num-fichas(lista)
	(loop for columna in lista count (not (null x))))

;;Busca posibilidades para el contrincante
(defun posibilidades-oponente(distancia consecutivas fichas posicion)
	(cond ((= 1 consecutivas) posicion)
		  ((< 3 consecutivas) (/ (* 7 +val-min+) posicion))
		  ((= 3 consecutivas) (* -1 (/ (* 7 +val-med+) posicion)))
		  (t (* -1 (- (* 7 fichas) posicion)))))

;;Busca posibilidades para el jugador
(defun posibilidades-actual(distancia consecutivas fichas posicion)
	(cond ((= 1 consecutivas) posicion)
		  ((< 3 consecutivas) (/ (* 7 +val-max+) posicion))
		  ((= 3 consecutivas) (* -1 (/ (* 7 +val-med+) posicion)))
		  (t (* -1 (- (* 7 fichas) posicion)))))

;;Calcular la distancia entre dos casillas
(defun distancia-entre (casilla1 casilla2)
	(cond ((equal casilla1 casilla2) 7)
		  ((eq (second casilla1) (second casilla2)) 1)
		  (t (abs (- (second casilla1) (second casilla2))))))

;;Devuelve la distancia minima entre dos casillas
(defun minimizar-distancia (lista posicion)
	(loop for columna in lista when (not (null x)) 
		minimize (distancia-entre columna posicion)))

;;Zonas interesantes
(defun zona-interesante(estado)
	(acciones-posibles estado))

;;Valora movimientos del jugador para hacer que pierda
(defun movimientos-clave-oponente(estado posicion)
	(let ((tablero (estado-tablero estado)))
	(loop for columna from 0 below (zona-interesante estado) when (> (length columna) 3)
		summing (posibilidades-oponente 
							   (minimizar-distancia columna posicion)
							   (consecutivas columna)
							   (num-fichas columna)
							   (busca-centro (second posicion))))))

;;Valora movimientos del jugador para ganar
(defun movimientos-clave-actual(estado posicion)
	(let ((tablero (estado-tablero estado)))
	(loop for columna from 0 below (zona-interesante estado) when (> (length columna) 3)
		summing (posibilidades-actual 
							   (minimizar-distancia columna posicion)
							   (consecutivas columna)
							   (num-fichas columna)
							   (busca-centro (second posicion))))))

;;Heuristica-base
(defun base-heuristica(estado posicion)
	(let* ((estrategia-ganar (movimientos-clave-actual estado posicion)))
		  ((estrategia-defensa (movimientos-clave-oponente estado posicion)))
		 (if (> (abs estrategia-ganar) (abs estrategia-defensa))
		 	estrategia-ganar
		 	estrategia-desfensa)))

;;Heuristica final
(defun laLeche(estado)
	(let ((tablero (estado-tablero estado)))
	(loop for posicion from 0 below (tablero-ancho tablero) 
		summing (base-heuristica estado posicion))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

(defun prueba11 (estado)
	(let* ((tablero (estado-tablero estado))
		(posiciones (columnas-jugables tablero))
		(ficha-actual (estado-turno estado))
		(nuevo-tablero (copiar-tablero estado)))
		(loop for columna from 0 below (tablero-ancho tablero) do
			(cond ((null posiciones)
					nil)
					(t (setf (aref nuevo-tablero (first posiciones) columna) ficha-actual) nuevo-tablero)))))

(defun prueba9 (estado)
	(let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual))) 
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (arriba (contar-arriba tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 15)
			       ((= abajo 2) 500)
			       ((= abajo 3) 3500))
			 (cond ((= arriba 0) 0)
			       ((= arriba 1) 15)
			       ((= arriba 2) 500)
			       ((= arriba 3) 3500))
			 (cond ((= der 0) 0)
			       ((= der 1) 15)
			       ((= der 2) 500)
			       ((= der 3) 3500))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 15)
			       ((= izq 2) 500)
			       ((= izq 3) 3500))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 15)
			       ((= abajo-izq 2) 500)
			       ((= abajo-izq 3) 3500))
			 (cond ((= abajo-der 0) 0)
			       ((= abajo-der 1) 15)
			       ((= abajo-der 2) 500)
			       ((= abajo-der 3) 3500))
			 (cond ((= arriba-der 0) 0)
			       ((= arriba-der 1) 15)
			       ((= arriba-der 2) 500)
			       ((= arriba-der 3) 3500))
			 (cond ((= arriba-izq 0) 0)
			       ((= arriba-izq 1) 15)
			       ((= arriba-izq 2) 500)
			       ((= arriba-izq 3) 3500))
			 )))
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-oponente columna fila))
		     (arriba (contar-arriba tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-oponente columna fila))
		     (izq (contar-izquierda tablero ficha-oponente columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila))
		     )
		(setf puntuacion-oponente
		      (+ puntuacion-oponente
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 200)
			       ((= abajo 3) 1900))
			 (cond ((= arriba 0) 0)
			       ((= arriba 1) 10)
			       ((= arriba 2) 200)
			       ((= arriba 3) 1900))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 200)
			       ((= der 3) 1900))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 200)
			       ((= izq 3) 1900))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 200)
			       ((= abajo-izq 3) 2500))
			 (cond ((= abajo-der 0) 0)
			       ((= abajo-der 1) 10)
			       ((= abajo-der 2) 200)
			       ((= abajo-der 3) 2500))
			 (cond ((= arriba-der 0) 0)
			       ((= arriba-der 1) 10)
			       ((= arriba-der 2) 200)
			       ((= arriba-der 3) 2500))
			 (cond ((= arriba-izq 0) 0)
			       ((= arriba-izq 1) 10)
			       ((= arriba-izq 2) 200)
			       ((= arriba-izq 3) 2500))
			 ))))
	(- puntuacion-actual puntuacion-oponente)))))

;; -------------------------------------------------------------------------------
;; Jugadores 
;; -------------------------------------------------------------------------------

(defvar *jugador-aleatorio* (make-jugador :nombre 'Jugador-aleatorio
					  :f-jugador #'f-jugador-aleatorio
					  :f-eval  #'f-eval-aleatoria))

(defvar *jugador-bueno* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'f-eval-bueno))

(defvar *jugador-humano* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-humano
				       :f-eval  #'f-no-eval))

(defvar *jugador-mio* (make-jugador :nombre 'Stark
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'prueba1))

(defvar *jugador-mio2* (make-jugador :nombre 'Targaryen
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'prueba2))

(defvar *jugador-mio3* (make-jugador :nombre 'Lannister
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'prueba3))

(defvar *jugador-mio4* (make-jugador :nombre 'Greyjoy
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'prueba4))

(defvar *jugador-mio5* (make-jugador :nombre 'Baratheon
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'prueba5))


(defvar *jugador-mio9* (make-jugador :nombre 'Mormont
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'prueba9))


(defvar *jugador-mio10* (make-jugador :nombre 'Mormont2
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'laLeche))

(defvar *jugador-mio11* (make-jugador :nombre 'Mormont
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'prueba11))
;; -------------------------------------------------------------------------------
;; Algunas partidas de ejemplo:
;; -------------------------------------------------------------------------------

(setf *verbose* t)

;(print (partida *jugador-aleatorio* *jugador-aleatorio*))
;(print (partida *jugador-aleatorio* *jugador-bueno* 4))
;(print (partida *jugador-bueno* *jugador-aleatorio* 4))
;(print (partida *jugador-bueno* *jugador-bueno* 4))
;(print (partida *jugador-humano* *jugador-humano*))
;(print (partida *jugador-humano* *jugador-aleatorio* 4))
;(print (partida *jugador-mio* *jugador-bueno* 4))
;(print (partida *jugador-aleatorio* *jugador-humano*))
;(print (partida *jugador-mio2* *jugador-aleatorio* 4))
;(print (partida *jugador-mio* *jugador-bueno* 4))
;;(print (partida *jugador-mio5* *jugador-aleatorio* 4))
(print (partida *jugador-mio11* *jugador-bueno* 4))