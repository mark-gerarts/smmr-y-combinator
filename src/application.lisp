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
    (draw element (screen application)))
  (event-case ((screen application) event)
    (#\q (return-from event-case))))
