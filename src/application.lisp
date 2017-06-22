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
  (let* ((scr (screen application))
         (scr-width (.width scr))
         (app-width 80)
         (start-x (if (<= scr-width app-width)
                      0
                      (floor (/ (- scr-width app-width) 2))))
         (elements (copy-list (elements application))))
    ;; Draw the header.
    (move scr 1 (+ start-x 2))
    (format scr "~A" "SMMR-Y-COMBINATOR")
    (fill-line scr :length app-width :start-pos `(,start-x 2))
    (add-offset-adjustment elements start-x 3 )
    (loop for element in elements do
      (draw element (screen application)))
    ;; Because render is called multiple times, we have to remove the adjustment
    ;; every time.
    ;; @todo: this logic should be replaced by something better.
    (remove-offset-adjustment elements start-x 3)))

(defun add-offset-adjustment (elements offset-x offset-y)
  "Loops over all elements and adds the given x and y offsets to the positions."
  (loop for el in elements do
    (let* ((new-x (+ (x el) offset-x))
           (new-y (+ (y el) offset-y)))
      (setf (pos el) (list new-x new-y)))))

(defun remove-offset-adjustment (elements offset-x offset-y)
  "Loops over all elements and removes the given x and y offsets from the
   positions."
  (add-offset-adjustment elements (* -1 offset-x) (* -1 offset-y)))

(defun get-selectable-elements (elements)
  "When given a list of elements, filters out those that are not selectable"
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
