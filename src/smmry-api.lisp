;;;; smmry-api.lisp
;;;;
;;;; All things related to the SMMRY API.

(in-package #:smmr-y-combinator)

(defvar *smmry-api-url* "http://api.smmry.com")

(defvar *smmry-api-key*
  (with-open-file (stream "~/.smmry-api-key")
    (read-line stream))
  "Fetch the API key by reading it from a file. @todo: make it possible to pass
   the API key as a parameter")

(defun summarize (url)
  "Summarize the given URL using the SMMRY API."
  (let* ((url (format nil "~A?SM_API_KEY=~A&SM_URL=~A"
                      *smmry-api-url*
                      *smmry-api-key*
                      url))
         (*header-stream* nil)
         (*text-content-types*
           (cons '("application" . "json") *text-content-types*))
         (response (decode-json-from-string (http-request url))))
    (cdr (assoc :sm--api--content response))))
