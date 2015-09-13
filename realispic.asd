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
               :jonathan
               :cl-fad)
  :components ((:module "src" :components
                        ((:file "candy")
                         (:file "compiler")
                         (:file "rpc")
                         (:file "widget" :depends-on ("compiler"))
                         (:file "css")
                         (:file "bootstrap")
                         (:file "application" :depends-on ("widget" "css" "rpc"))
                         (:file "realispic" :depends-on ("application" "bootstrap" "rpc" "candy")))))
  :description "A simple web framework based on clack, parenscript and React.js.")
  
  
