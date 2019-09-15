(defpackage	:2311_P08_c3f45
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alres*))
	
(in-package	2311_P08_c3f45)

(defvar *alias* 'Lannister)

(defun prueba3 (estado)
	(random estado))

(defun heuristica (estado)
	(prueba3 estado))