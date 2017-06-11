;;;; elements.lisp
;;;;
;;;; Provides classes that represents elements on the screen.

(in-package #:smmr-y-combinator)

(defclass element ()
  ((pos
    :initarg :pos
    :initform '(0 0)
    :accessor pos
    :documentation "The element's position on the screen, represented as the
                    tuple x and y.")
   (selectable
    :initarg :selectable
    :accessor selectable
    :documentation "Wether or not the element is selectable by the user.")
   (selected
    :initarg :selected
    :initform nil
    :accessor is-selected
    :documentation "Wether or not the element is selected by the user."))
  (:documentation "Represents an element that can be printed on the screen."))

(defgeneric draw (element scr))

(defclass item (element)
  ((data
    :initarg :data
    :initform (error "Data must not be empty")
    :accessor data
    :documentation "The raw data retrieved from the api")
   (index
    :initarg :index
    :initform 0
    :accessor index))
  (:documentation "An element that represents a HN item"))

(defmethod draw ((item item) scr)
  (format scr "~A~A: ~A"
          (if (is-selected item) "> " "  ")
          (index item)
          (get-property item :title)))

(defgeneric get-property (item property)
  (:documentation "Retrieves an item's property by name"))

(defmethod get-property ((item item) property)
  (cdr (assoc property (data item))))
