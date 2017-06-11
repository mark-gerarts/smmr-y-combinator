;;;; smmr-y-combinator.lisp

(in-package #:smmr-y-combinator)

(defvar *stories* (fetch-item-list (get-top-stories) 5))

(defun draw-items (scr items &optional (active 0))
  "Draws the given items on the screen. The active item will be highlighted."
  (clear scr)
  (loop for item in items
        for i from 0 do
          (move scr i 0)
          (format scr "~A~A: ~A"
                  (if (= i active) "> " "  ")
                  (1+ i)
                  (cdr (assoc :title item)))
          (refresh scr)))

(defun init ()
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :cursor-visibility nil
                    :enable-colors t)
    (let* ((items *stories*)
           (n (length items))
           (i 0))
      (draw-items scr items)
      (event-case (scr event)
        (:up (setf i (mod (1- i) n)) (draw-items scr items i))
        (:down (setf i (mod (1+ i) n)) (draw-items scr items i))
        (#\q (return-from event-case))))))
