;;;; application.lisp

(in-package #:smmr-y-combinator)

(defclass application ()
  ((screen
    :initarg :screen
    :initform nil
    :accessor screen)
   (elements
    :initarg :elements
    :initform '()
    :accessor elements))
  (:documentation "Handles the rendering of elements on a screen."))

(defgeneric render (application)
  (:documentation "Renders the application on the attached screen."))

(defmethod render ((application application))
  (loop for element in (elements application) do
    (draw element (screen application))))

(defgeneric select-next (application)
  (:documentation "Makes the next element selected."))

(defmethod select-next ((application application))
  (let* ((elements (elements application))
         (selectable-elements (remove-if-not
                               #'(lambda (el) (selectable el))
                               elements))
         (selected-element (find-if
                            #'(lambda (el) (is-selected el))
                            selectable-elements))
         (next-element (nth (mod (1+ (position selected-element elements))
                                 (length elements))
                            elements)
                       ))
    (setf (is-selected selected-element) nil)
    (setf (is-selected next-element) t)))
