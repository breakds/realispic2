(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic)
    (defpackage realispic
      (:use :cl)
      (:import-from :realispic.application
                    :def-app
                    :*template-path*)
      (:import-from :realispic.widget
                    :def-widget
                    :import-widget)
      (:import-from :realispic.bootstrap
                    :bootstrap)
      (:import-from :realispic.rpc
                    :def-service
                    :with-rpc
                    :rpc-result
                    :rpc-error)
      (:export :def-app
               :def-widget
               :def-service
               :import-widget
               :bootstrap
               :*template-path*
               :with-rpc
               :rpc-result
               :rpc-error))))
(in-package :realispic)
