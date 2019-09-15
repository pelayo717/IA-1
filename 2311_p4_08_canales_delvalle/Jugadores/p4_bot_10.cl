(defpackage	:2311_P08_ef12d
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))
	
(in-package	2311_P08_ef12d)

(defvar *alias* 'prueba14)

(defun prueba10(estado)
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
				(cond ((null acciones) 0)
					(t (setf  puntuacion-asc 
						(+ puntuacion-asc 
						(cond ((= num-fichas-diagonal-asc 0) 0)
							((= num-fichas-diagonal-asc 1) 100)
							((= num-fichas-diagonal-asc 2) 200)
							((= num-fichas-diagonal-asc 3) 3000)
							(t (+ puntuacion-desc 10)))))
					(setf  puntuacion-desc 
						(+ puntuacion-desc 
						(cond ((= num-fichas-diagonal-desc 0) 3000)
							((= num-fichas-diagonal-desc 1) 200)
							((= num-fichas-diagonal-desc 2) 100)
							((= num-fichas-diagonal-desc 3) 0)
							(t (+ puntuacion-desc 10)))))))
			
			))

		(* (- 0 1) (- puntuacion-desc puntuacion-asc))

	))

(defun heuristica (estado)
	prueba10 estado)