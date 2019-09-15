(defpackage	:2311_P08_ef12d
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))
	
(in-package	2311_P08_ef12d)

(defvar *alias* 'Baratheon)

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



(defun heuristica (estado)
	(prueba5 estado))