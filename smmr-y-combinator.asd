;;;; smmr-y-combinator.asd

(asdf:defsystem #:smmr-y-combinator
  :description "Browse and summarize HN in your terminal"
  :author "Mark Gerarts <mark.gerarts@gmail.com>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (#:croatoan
               #:drakma
               #:cl-json
               #:trivial-open-browser)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "hacker-news-api")
                             (:file "smmry-api")
                             (:file "elements")
                             (:file "application")
                             (:file "smmr-y-combinator")))))
