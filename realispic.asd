(in-package :cl-user)
(defpackage realispic-asd
  (:use :cl :asdf))
(in-package :realispic-asd)

(defsystem realispic
  :version "2.0.0"
  :author "BreakDS <breakds@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria 
               :parenscript 
               :clack 
               :lack
               :hunchentoot
               :html-template
               :cl-template
               :cl-fad)
  :components ((:module "src" :components
                        ((:file "compiler")
                         (:file "widget" :depends-on ("compiler"))
                         (:file "css")
                         (:file "bootstrap")
                         (:file "application" :depends-on ("widget" "css"))
                         (:file "realispic" :depends-on ("application" "bootstrap")))))
  :description "A simple web framework based on clack, parenscript and React.js.")
  
  
