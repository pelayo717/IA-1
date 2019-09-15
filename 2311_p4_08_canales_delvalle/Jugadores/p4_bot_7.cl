(defpackage	:2311_P08_ef12d
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))
	
(in-package	2311_P08_ef12d)

(defvar *alias* '|LG02|)

(defun prueba7 (estado)
	(let* ((tablero (estado-tablero estado))
			(ficha-actual (estado-turno estado))
			(ficha-oponente (siguiente-jugador ficha-actual))
			(acciones (acciones-posibles estado))
			(puntuacion-asc 0)
			(puntuacion-desc 0))
		(loop for columna from 0 below (tablero-ancho tablero) do
			(let* ((altura (altura-columna tablero columna))
				(fila (1- altura))
				(num-fichas-diagonal-asc (+ (contar-abajo-izquierda tablero ficha-oponente columna fila)
											(contar-arriba-derecha tablero ficha-oponente (1+ columna) (1+ fila))))
				(num-fichas-diagonal-desc (+ (contar-abajo-derecha tablero ficha-oponente columna fila)
											 (contar-arriba-izquierda tablero ficha-oponente (1- columna) (1+ fila)))))
				(cond ((null acciones) nil)
					(t (setf  puntuacion-asc 
						(+ puntuacion-asc 
						(cond ((= num-fichas-diagonal-asc 0) 0)
							((= num-fichas-diagonal-asc 1) 5)
							((= num-fichas-diagonal-asc 2) 10)
							((= num-fichas-diagonal-asc 3) 15))))
					(setf  puntuacion-desc 
						(+ puntuacion-desc 
						(cond ((= num-fichas-diagonal-desc 0) 15)
							((= num-fichas-diagonal-desc 1) 10)
							((= num-fichas-diagonal-desc 2) 5)
							((= num-fichas-diagonal-desc 3) 0))))))
			
			))

		(+ puntuacion-desc puntuacion-asc)

	))

(defun heuristica (estado)
	(prueba7 estado))