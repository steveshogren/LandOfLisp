; Simple number guessing game
; Started with: start-over()
(defun get-midpoint (small big)
  (ash (+ small big) -1))

(defun ask-if-bigger-or-smaller (small big)
  (format t "Is your number bigger or smaller?")
  (setf input read)
  (case input
	((bigger) (bigger(small big)))
	((smaller) (smaller(small big)))
	(otherwise (smaller(small big)))))

(defun smaller (small big)
  (ask-if-bigger-or-smaller(small (1-(get-midpoint(small big))))))

(defun bigger (small big)
  (ask-if-bigger-or-smaller(small (1-(get-midpoint(small big))))))

(defun start-over()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (ask-if-bigger-or-smaller(*small* *big*)))

; Case Statements
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (case person
	((henry)   (setf *arch-enemy* 'stupid-lisp-alien)
	            '(curse you lisp alien - you ate my pudding))
	((johnny)  (setf *arch-enemy* 'useless-old-johnny)
 	            '(i hope you choked on my pudding johnny))
	(otherwise '(why you eat my pudding stranger?))))

; Text game engine
(defparameter *nodes* '((living-room(you are in the living room.
					 a wizard is snoring on the couch.))
			(garden (you are in a mysterious garden.
				     there is a well in front of you.))
			(attic (you are in the attic.
				    there is a giant welding torch in the corner.))))
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)
				     (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))