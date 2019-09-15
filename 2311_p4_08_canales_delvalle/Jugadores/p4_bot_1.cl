(defpackage	:2311_P08_c3f45
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alres*))
	
(in-package	2311_P08_c3f45)

(defvar *alias* 'Targaryen)

(defun prueba1 (estado)
	(if (juego-terminado-p estado)
		+val-max+
		+val-min+))

(defun heuristica (estado)
	(prueba1 estado))