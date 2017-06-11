;;;; smmr-y-combinator.lisp

(in-package #:smmr-y-combinator)

(defvar *items* (mapcar #'(lambda (story)
                            (make-instance 'item :data story))
                        (fetch-item-list (get-top-stories) 2)))

(defun draw-items (scr items)
  "Draws the given items on the screen. The active item will be highlighted."
  (clear scr)
  (loop for item in items
        for i from 0 do
          (move scr i 0)
          (draw item scr)
          (refresh scr)))

(defun init ()
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :cursor-visibility nil
                    :enable-colors t)
    (let ((items *items*))
      (draw-items scr items)
      (event-case (scr event)
        (#\q (return-from event-case))))))
