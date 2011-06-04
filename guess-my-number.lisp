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

(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
		     (eq (cadr (assoc obj obj-locs)) loc)))
	  (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
	  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
      '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun drop (object)
  (cond ((member object
		 (objects-at 'body *objects* *object-locations*))
	 (push (list object *location*) *object-locations*)
	 `(you drop the ,object))
	(t '(you do not have that to drop.))))
