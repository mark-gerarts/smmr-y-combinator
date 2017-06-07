;;;; smmr-y-combinator.asd

(asdf:defsystem #:smmr-y-combinator
  :description "Browse and summarize HN in your terminal"
  :author "Mark Gerarts <mark.gerarts@gmail.com>"
  :license "GPLv3"
  :depends-on (#:croatoan
               #:drakma)
  :serial t
  :components ((:file "package")
               (:file "hacker-news-api")
               (:file "smmr-y-combinator")))
