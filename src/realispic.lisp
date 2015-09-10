(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic)
    (defpackage realispic
      (:use :cl)
      (:import-from :realispic.application
                    :def-app)
      (:import-from :realispic.widget
                    :def-widget
                    :import-widget)
      (:import-from :realispic.bootstrap
                    :bootstrap)
      (:export :def-app
               :def-widget
               :import-widget
               :bootstrap
               :*template-path*))))
(in-package :realispic)
