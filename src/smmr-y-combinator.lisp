;;;; smmr-y-combinator.lisp

(in-package #:smmr-y-combinator)

(defun elements-from-story (story index)
  "Creates the item element and its associated buttons for a story."
  (let* ((item (make-instance 'item
                              :data story
                              :index index
                              :pos `(,(* 2 index) 0)))
         (summarize (make-instance 'button
                                   :pos `(,(1+ (* 2 index)) 4)
                                   :label "Summarize")))
    (list item summarize)))

(defun get-initial-elements ()
  "Creates a list of elements that should be available when the application
   loads."
  (loop for story in (fetch-item-list (get-top-stories) 2)
        for i from 0 append (elements-from-story story i)))

(defun init ()
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :cursor-visibility nil
                    :enable-colors t)
    (let* ((elements (get-initial-elements))
           (application (make-instance 'application
                                       :screen scr
                                       :elements elements)))
      ;; Set the first element as selected by default.
      (setf (is-selected (first elements)) t)
      (render application)
      (event-case (scr event)
        ;; @todo: add :up as well
        (:down (select-next application) (render application))
        (#\q (return-from event-case))))))
