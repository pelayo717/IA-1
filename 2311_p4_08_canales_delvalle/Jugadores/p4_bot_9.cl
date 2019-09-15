(defpackage	:2311_P08_2742e
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))
	
(in-package	2311_P08_2742e)

(defvar *alias* '|LG01|)

(defun prueba9 (estado)
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
					(setf puntuacion-actual (+ puntuacion-actual 50)))
				(if (= num-fichas 2)
					(setf puntuacion-oponente (+ puntuacion-oponente 50))
					(setf puntuacion-actual (+ puntuacion-actual 25)))
				(if (= num-fichas 1)
					(setf puntuacion-oponente (+ puntuacion-oponente 25))
					(setf puntuacion-actual (+ puntuacion-actual 20)))))
		(- puntuacion-oponente puntuacion-actual)))


(defun heuristica (estado)
	(prueba9 estado))