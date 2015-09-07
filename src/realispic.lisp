(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic)
    (defpackage realispic
      (:import-from :realispic.application
                    :def-app)
      (:import-from :realispic.widget
                    :def-widget
                    :import-widget)
      (:export :def-app
               :def-widget
               :import-widget))))
(in-package :realispic)
