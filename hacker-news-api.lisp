;;;; hacker-news-api.lisp

(in-package #:smmr-y-combinator)

(defvar *hn-base-uri* "https://hacker-news.firebaseio.com/v0/")

(defun build-hn-url (endpoint &optional parameter)
  (concatenate
   'string
   *hn-base-uri*
   endpoint
   (if parameter (concatenate 'string "/" parameter))
   ".json"))

(defun do-request (endpoint &optional parameter)
  (let ((*header-stream* nil)
        ;; The content type has to be specified in otder for drakma
        ;; to return the value as text.
        (*text-content-types*
          (cons '("application" . "json") *text-content-types*)))
    (http-request (build-hn-url endpoint parameter))))

;;; The following functions represent the endpoints of the API.

(defun get-item (id)
  (do-request "item" (write-to-string id)))

(defun get-user (id)
  (do-request "user" id))

(defun get-max-item ()
  (do-request "maxitem"))

(defun get-top-stories ()
  (do-request "topstories"))

(defun get-new-stories ()
  (do-request "newstories"))

(defun get-best-stories ()
  (do-request "beststories"))

(defun get-ask-stories ()
  (do-request "askstories"))

(defun get-show-stories ()
  (do-request "showstories"))

(defun get-job-stories ()
  (do-request "jobstories"))

(defun get-updates ()
  (do-request "updates"))
