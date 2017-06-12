;;;; smmr-y-combinator.lisp

(in-package #:smmr-y-combinator)

(defun init ()
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :cursor-visibility nil
                    :enable-colors t)
    (let* ((items (mapcar #'(lambda (story) (make-instance 'item :data story))
                          (fetch-item-list (get-top-stories) 2)))
           (application (make-instance 'application
                                       :screen scr
                                       :elements *items*)))
      (render application))))
