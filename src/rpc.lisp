(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.rpc)
    (defpackage realispic.rpc
      (:use :cl :parenscript)
      (:import-from :alexandria
                    :with-gensyms
                    :symbolicate
                    :when-let
                    :if-let)
      (:import-from :jonathan
                    :to-json)
      (:export :def-service
               :rpc-middleware
               :*realispic-service-table*
               :with-rpc
               :rpc-result
               :rpc-error))))
(in-package :realispic.rpc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Use defparameter since we would like to get rid of deleted
  ;; services upon reload the ASDF system.
  (defparameter *realispic-service-table* (make-hash-table :test #'equal)
    "Map from services names (string) to the RPC object.")

  (declaim (inline match-prefix))
  (defun match-prefix (text prefix)
    "Check whether PREFIX is actually a prefix of TEXT. The match is
     not case sensitive. Returns nil on no-match."
    (and (>= (length text) (length prefix))
         (string-equal prefix (subseq text 0 (length prefix)))))

  (declaim (inline with-prefix-off))
  (defun prefix-off (text prefix)
    "Returns the part of TEXT without PREFIX if TEXT actually starts
     with PREFIX. Returns NIL otherwise."
    (and (match-prefix text prefix)
         (subseq text (length prefix))))

  (defun rpc-middleware (app)
    (lambda (env)
      (if-let ((rpc-name (prefix-off (getf env :path-info) "/services/")))
        (if-let ((service (gethash (string-upcase rpc-name) 
                                *realispic-service-table*
                                nil)))
          (handler-case 
              (funcall service (getf env :query-parameters))
            (t (error-object) 
              `(500 (:content-type "text/plain")
                    (,(format nil "Internal Server Error: ~a"
                              error-object)))))
          `(404 (:content-type "text/plain")
                (,(format nil "Service ~a not found." 
                          rpc-name))))
        (funcall app env)))))
          
(defmacro def-service (name args &body body)
  (with-gensyms (query-parameters slot)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ,(mapcar (lambda (arg)
                               (etypecase arg
                                 (symbol arg)
                                 (cons (car arg))))
                             args)
         ,@body)
       (setf (gethash (symbol-name ',name) *realispic-service-table*)
             ;; The wrapper over the original function that
             ;; 1) Parses the query parameters
             ;; 2) Pass those parameters to the actual function call
             ;; 3) Convert the result into JSON
             (lambda (,query-parameters)
               (list 200
                     '(:content-type "application/json")
                     (to-json 
                      (,name ,@(mapcar (lambda (arg)
                                         (etypecase arg
                                           (symbol `(when-let ((,slot (assoc ,(string-downcase
                                                                               (symbol-name arg))
                                                                             ,query-parameters
                                                                             :test #'equal)))
                                                      (cdr ,slot)))))
                                       args)))))))))

(defpsmacro with-rpc ((service-name &rest args) &body body)
  `(funcall (@ j-query ajax)
            (create :url ,(format nil "/services/~a" service-name)
                    :method "GET"
                    :data (create ,@args)
                    :complete (chain (lambda (xhr status)
                                       (let ((rpc-result (@ xhr response-j-s-o-n))
                                             (rpc-error (when (!= status "success")
                                                          (@ xhr response-text))))
                                         ,@body))
                                     (bind this)))))
