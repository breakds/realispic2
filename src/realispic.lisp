(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic)
    (defpackage realispic
      (:import-from :realispic.application
                    :def-app)
      (:export :def-app))))
(in-package :realispic)
