(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.candy)
    (defpackage realispic.candy
      (:use :cl :parenscript)
      (:export :lambda!
               :map
               :random-int
               :trace))))
(in-package :realispic.candy)

(defpsmacro lambda! (args &body body)
  `(chain (lambda ,args ,@body)
          (bind this)))
  
(defpsmacro map (func list &optional (this 'this))
  `((@ ,list map) ,func ,this))

(defpsmacro random-int (x)
  `(funcall (@ *math floor)
            (* (funcall (@ *math random)) ,x)))

(defpsmacro trace (content)
  `((@ console log) ,content))




  

