(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.application)
    (defpackage realispic.application
      (:use :cl :parenscript)
      (:import-from :alexandria
                    :with-gensyms
                    :symbolicate))))
(in-package :realispic.application)

(defmacro def-app (app-name args 
                   &key
                     (server :hunchentoot)
                     (title "Undefined Name")
                     (port 5000)
                     (static-path "/asset/")
                     (static-root "/tmp/")
                     (icon "")
                     (includes nil))
  ;; TODO(breakds): ensure that static-path and static-root are
  ;; directory like (tailing slash).
  (with-gensyms (env)
    (let ((app-function-name (symbolicate '* app-name '-app*))
          (app-handler-name (symbolicate '* app-name '-handler '*)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; 2. Assemble the application
         (defparameter ,app-function-name
           (lack:builder 
            :session 
            (:static :path ,static-path
                     :root ,static-root)
            (lambda (env)
              '(200 (:content-type "text/plain") ("haha" "hehe"))))
           "The global variable that holds the application function.")
         ;; 3. Create the app controller
         (defvar ,app-handler-name nil
           "The global variable that holds the application handler.")
         (defun ,app-name (&optional (command :start))
           (ecase command
             (:start (setf ,app-handler-name
                           (clack:clackup ,app-function-name
                                          :server ,server)))
             (:stop (clack:stop ,app-handler-name))))))))
                     
                 
                     
                                   

                         
      
    
