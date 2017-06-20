;;;; screen.lisp
;;;;
;;;; Helper methods for screen rendering.

(in-package #:smmr-y-combinator)

(defun fill-line (scr &key
                        (start-pos '(0 0))
                        (length nil)
                        (char #\-))
  "Fills a single line of the screen with the given char."
  (let ((x (car start-pos))
        (y (cadr start-pos))
        ;; If no length is given, we'll take the full width of the screen.
        (length (if length length (.width scr))))
    (move scr y x)
    ;; See https://stackoverflow.com/questions/24754552 for some explanation of
    ;; this format syntax.
    (format scr "~v@{~A~:*~}" length char)))
