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

(defun get-selectable-elements (elements)
  "When given a list of elements, filters out thos who are not selectable"
  (remove-if-not #'(lambda (el) (selectable el)) elements))

(defun get-selected-element (elements)
  "When given a list of elements, returns the first one that is selected"
  (find-if #'(lambda (el) (is-selected el)) elements))

(defgeneric select-next (application)
  (:documentation "Marks the next element as selected."))

(defmethod select-next ((application application))
  (let* ((elements (elements application))
         (selectable-elements (get-selectable-elements elements))
         (selected-element (get-selected-element elements))
         (next-element (nth (mod (1+ (position selected-element elements))
                                 (length elements))
                            elements)))
    (setf (is-selected selected-element) nil)
    (setf (is-selected next-element) t)))

(defgeneric select-previous (application)
  (:documentation "Marks the previous element as selected."))

(defmethod select-previous ((application application))
  (let* ((elements (elements application))
         (selectable-elements (get-selectable-elements elements))
         (selected-element (get-selected-element elements))
         (prev-element (nth (mod (1- (position selected-element elements))
                                 (length elements))
                            elements)))
    (setf (is-selected selected-element) nil)
    (setf (is-selected prev-element) t)))
