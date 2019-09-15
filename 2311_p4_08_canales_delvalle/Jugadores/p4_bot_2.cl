(defpackage	:2311_P08_c3f45
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alres*))
	
(in-package	2311_P08_c3f45)

(defvar *alias* 'Stark)

(defun prueba2 (estado)
	(if (generar-sucesores estado)
		0
		1))

(defun heuristica (estado)
	(prueba2 estado))