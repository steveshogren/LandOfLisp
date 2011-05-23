(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))


(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (case person
	((henry)   (setf *arch-enemy* 'stupid-lisp-alien)
	            '(curse you lisp alien - you ate my pudding))
	((johnny)  (setf *arch-enemy* 'useless-old-johnny)
 	            '(i hope you choked on my pudding johnny))
	(otherwise '(why you eat my pudding stranger?))))
