;;;; smmr-y-combinator.lisp

(in-package #:smmr-y-combinator)

(defun elements-from-story (story index)
  "Creates the item element and its associated buttons for a story."
  (let* ((item (make-instance
                'item
                :data story
                :index index
                :pos `(,(* 2 index) 0)
                :interactable t))
         (next-row (1+ (* 2 index)))
         (summarize (make-instance 'button
                                   :pos (list next-row 4)
                                   :label "Summarize"))
         (comments
           (make-instance 'button
                          :pos (list next-row 15)
                          :label "Comments"
                          :action `(open-browser ,(get-comments-url item))
                          :interactable t)))
    (setf (action item) `(open-browser ,(get-property item :url)))
    (list item summarize comments)))

(defun get-initial-elements ()
  "Creates a list of elements that should be available when the application
   loads."
  (loop for story in '() ;(fetch-item-list (get-top-stories) 2)
        for i from 0 append (elements-from-story story i)))

(defun init ()
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :cursor-visibility nil
                    :enable-colors t)
    (let* ((elements (get-initial-elements))
           (first-el (first elements))
           (application (make-instance 'application
                                       :screen scr
                                       :elements elements)))
      ;; Set the first element as selected by default.
      (when first-el (setf (selected-p first-el) t))
      (render application)
      (event-case (scr event)
                  (:up (select-previous application) (render application))
                  (:down (select-next application) (render application))
                  (#\newline (do-action application))
                  (#\q (return-from event-case))))))
