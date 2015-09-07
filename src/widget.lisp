(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.widget)
    (defpackage realispic.widget
      (:use :cl :parenscript :realispic.compiler)
      (:import-from :alexandria
                    :with-gensyms
                    :symbolicate)
      (:export :def-widget
               :import-widget
               :compile-psx
               :*realispic-widget-table*))))
(in-package :realispic.widget)

;; TODO(breakds): widget should have two names - the true name which
;; is a gensym so that it is very unlikely to pollute the javascript
;; global scope, and a simple name to be used in lisp.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *realispic-widget-table* (make-hash-table :test #'equal)
    "Stores all the realispic (custom) symbols and its value, where
    the value is the function that generates ParenScript code and its
    dependencies list."))

(defmacro def-widget (name (&rest args) &body body)
  (multiple-value-bind (attribute-names state-defs)
      (split-argument-list args)
    (multiple-value-bind (compiled-body dependencies css)
	(compile-psx (if (= (length body) 1)
			 (car body)
			 `(progn ,@(car body)))
		     :attribute-names attribute-names
		     :state-defs state-defs)
      (with-gensyms (language)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (defun ,name (&optional (,language :javascript))
             (if (eq ,language :css)
                 ',css
                 `(defvar ,',name ((@ *react create-class)
                                   (create get-initial-state ,',(initial-state-slot state-defs)
                                           ,@',compiled-body)))))
           (setf (gethash (symbol-name ',name) *realispic-widget-table*)
                 (cons #',name ',dependencies)))))))

(defmacro import-widget (name &key
                                (original-name nil)
                                (namespaces nil))
  (with-gensyms (language)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defun ,name (&optional (,language :javascript))
           (ecase ,language
             (:css nil)
             (:javascript `(defvar ,',name (aref this 
                                                 ,,@namespaces 
                                                 ,',original-name)))))
         (setf (gethash (symbol-name ',name) *realispic-widget-table*) 
               (list #',name))))))


           
                                
                                     
