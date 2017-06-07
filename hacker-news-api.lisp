;;;; hacker-news-api.lisp

(in-package #:smmr-y-combinator)

(defvar *hn-base-uri* "https://hacker-news.firebaseio.com/v0/")

(defun do-request (endpoint)
  (let ((*header-stream* nil)
        (*text-content-types*
          (cons '("application" . "json") *text-content-types*))
    (http-request (concatenate 'string *hn-base-uri* endpoint))))
