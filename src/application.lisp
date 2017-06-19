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
  (remove-if-not #'(lambda (el) (selectable-p el)) elements))

(defun get-selected-element (elements)
  "When given a list of elements, returns the first one that is selected"
  (find-if #'(lambda (el) (selected-p el)) elements))

(defun select-next (application)
  "Marks the next element as selected"
  (let* ((elements (elements application))
         (selectable-elements (get-selectable-elements elements))
         (selected-element (get-selected-element elements))
         (next-element (nth (mod (1+ (position selected-element elements))
                                 (length elements))
                            elements)))
    (setf (selected-p selected-element) nil)
    (setf (selected-p next-element) t)))

(defun select-previous (application)
  "Marks the previous element as selected"
  (let* ((elements (elements application))
         (selectable-elements (get-selectable-elements elements))
         (selected-element (get-selected-element elements))
         (prev-element (nth (mod (1- (position selected-element elements))
                                 (length elements))
                            elements)))
    (setf (selected-p selected-element) nil)
    (setf (selected-p prev-element) t)))

(defun do-action (application)
  "Performs an action with the currently selected element, if possible."
  (let ((selected-el (get-selected-element (elements application))))
    (when (and selected-el (interactable-p selected-el))
      (let ((action (action selected-el)))
        (eval action)))))
