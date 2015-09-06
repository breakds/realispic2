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
               :hunchentoot)
  :components ((:module "src" :components
                        ((:file "compiler")
                         (:file "widget" :depends-on ("compiler"))
                         (:file "application" :depends-on ("widget"))
                         (:file "realispic" :depends-on ("application")))))
  :description "A simple web framework based on clack, parenscript and React.js.")
  
  