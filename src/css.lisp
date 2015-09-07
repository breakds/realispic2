(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.css)
    (defpackage realispic.css
      (:use :cl :parenscript)
      (:import-from :realispic.widget
                    :compile-psx
                    :*realispic-widget-table*)
      (:import-from :alexandria
                    :with-gensyms
                    :symbolicate)
      (:export :compile-css))))
(in-package :realispic.css)

;;; Currently using a simplified CSS syntax.
;;;
;;; CSS-RULE = (CSS-SELECTOR KEY-VALUE-PAIR*)
;;; KEY-VALUE-PAIR = KEY: VALUE
;;; 
;;; Special Instructions (@-rules)
;;; KEYFRAMES = (:keyframes name CSS-RULE*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun match-prefix (key prefix)
    (let ((key-string (mkstr key)))
      (and (> (length key-string) (length prefix))
           (string-equal (subseq key-string 0 (length prefix))
                         prefix))))
  
  (defun normalize-key (key)
    (cond ((match-prefix key "animation-")
           (let ((postfix (subseq (mkstr key) (length "animation-"))))
             (list (format nil "-webkit-animation-~a" postfix)
                   key)))
          (t (list key))))

  (defun emit-key-value (key value stream &optional (indentation 0))
    (loop for actual-key in (normalize-key key) do
         (format stream "~v@{~a ~:*~}" indentation " ")
         (format stream "~a: ~a;~%" actual-key value)))
  
  (defun emit-css-rule (css-rule stream &optional (indentation 0))
    (fresh-line stream)
    (format stream "~v@{~a ~:*~}" indentation " ")
    (format stream "~{~a ~}{~%" (car css-rule))
    (loop for (key value) on (cdr css-rule) by #'cddr
       do (emit-key-value key value stream (+ indentation 2)))
    (format stream "~v@{~a ~:*~}}~%" indentation " "))
  
  (defun emit-css-keyframes (css-rule stream &optional (indentation 0))
    (labels ((emit-intnl (prefix)
               (fresh-line stream)
               (format stream "~v@{~a ~:*~}" indentation " ")
               (format stream "@~akeyframes ~a {~%" 
                       prefix (cadr css-rule))
               (loop for rule in (cddr css-rule)
                  do (emit-css-rule rule stream (+ indentation 2)))
               (format stream "~v@{~a ~:*~}}~%" indentation " ")))
      (emit-intnl "")
      (emit-intnl "-moz-")
      (emit-intnl "-o-")
      (emit-intnl "-webkit-")))

  (defun emit-css (css stream &optional (indentation 0))
    (loop for rule in css
       do (case (car rule)
            (:keyframes (emit-css-keyframes rule stream indentation))
            (t (emit-css-rule rule stream indentation)))))

  (defun compile-css (css)
    (let ((stream (make-string-output-stream)))
      (emit-css css stream 4)
      (get-output-stream-string stream))))

