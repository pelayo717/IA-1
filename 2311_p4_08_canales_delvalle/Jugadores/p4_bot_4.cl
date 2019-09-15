(defpackage	:2311_P08_2742e
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))
	
(in-package	2311_P08_2742e)

(defvar *alias* 'Greyjoy)

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

(defun heuristica (estado)
	(prueba4 estado))