;;;; elements.lisp
;;;;
;;;; Provides classes that represents elements on the screen.

(in-package #:smmr-y-combinator)

;;; Element.

(defclass element ()
  ((pos
    :initarg :pos
    :initform '(0 0)
    :accessor pos
    :documentation "The element's position on the screen, represented as the
                    tuple x and y.")
   (selectable
    :initarg :selectable
    :accessor selectable-p
    :initform 't
    :documentation "Wether or not the element is selectable by the user.")
   (selected
    :initarg :selected
    :initform nil
    :accessor selected-p
    :documentation "Wether or not the element is selected by the user.")
   (interactable
    :initarg :interactable
    :initform nil
    :accessor interactable-p
    :documentation "Whether or not the element is interactable, for example a
                    button."))
  (:documentation "Represents an element that can be printed on the screen."))

(defgeneric draw (element scr)
  :documentation "Draws the element on the screen.")

(defgeneric x (element))

(defmethod x ((element element))
  (first (pos element)))

(defgeneric y (element))

(defmethod y ((element element))
  (second (pos element)))

;;; Button

(defclass button (element)
  ((action
    :initarg :action
    :accessor action
    :documentation "The action that will be performed when the button is
                    activated.")
   (label
    :initarg :label
    :accessor label
    :documentation "The text that will be printed."))
  (:documentation "Represents an interactable button"))

(defmethod draw ((button button) scr)
  (move scr (x button) (y button))
  (format scr "~A~A"
          (if (selected-p button) ">" " ")
          (label button)))

;;; HN item.

(defclass item (button element)
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
  (move scr (x item) (y item))
  (format scr "~A~A: ~A"
          (if (selected-p item) "> " "  ")
          (index item)
          (get-property item :title)))

(defgeneric get-property (item property)
  (:documentation "Retrieves an item's property by name"))

(defmethod get-property ((item item) property)
  (cdr (assoc property (data item))))

(defun get-comments-url (item)
  "Returns the link to the HN comments page of the item"
  (format nil "~Aitem?id=~A" *hn-base-uri* (get-property item :id)))
