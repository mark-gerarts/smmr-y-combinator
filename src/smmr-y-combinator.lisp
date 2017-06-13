;;;; smmr-y-combinator.lisp

(in-package #:smmr-y-combinator)

(defun stories-to-items (stories)
  (loop for story in stories
        for i from 0 collect
        (make-instance 'item :data story :pos `(,i 0))))

(defun init ()
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :cursor-visibility nil
                    :enable-colors t)
    (let* ((items (stories-to-items (fetch-item-list (get-top-stories) 2)))
           (application (make-instance 'application
                                       :screen scr
                                       :elements items)))
      ;; Set the first element as selected by default.
      (setf (is-selected (first items)) t)
      (render application)
      (event-case (scr event)
        (:down (select-next application) (render application))
        (#\q (return-from event-case))))))
