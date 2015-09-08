(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.bootstrap)
    (defpackage realispic.bootstrap
      (:use :cl :parenscript :realispic.compiler)
      (:import-from :alexandria
                    :with-gensyms
                    :symbolicate)
      (:export :bootstrap))))
(in-package :realispic.bootstrap)


(defun generate-from-template (template-path target-path variables)
  (ensure-directories-exist target-path)
  (with-open-file (input template-path
                         :direction :input
                         :if-does-not-exist :error)
    (let ((template (make-string (file-length input))))
      (read-sequence template input)
      (with-open-file (output target-path
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede)
        (write-string (funcall (cl-template:compile-template template)
                               variables)
                      output)))))

(defun generate-asd-file (name base)
  (generate-from-template (merge-pathnames "skeleton/project.asd.tmpl"
                                           (asdf:system-source-directory :realispic))
                          (merge-pathnames (format nil "~a.asd" name)
                                           base)
                          (list :name name)))

(defun generate-lisp-file (name base)
  (generate-from-template (merge-pathnames "skeleton/app.lisp.tmpl"
                                           (asdf:system-source-directory :realispic))
                          (merge-pathnames "app.lisp"
                                           base)
                          (list :name name
                                :generated-root (format nil "/tmp/~a/" name))))

(defun generate-js-file (name base)
  (generate-from-template (merge-pathnames "skeleton/app.js.tmpl"
                                           (asdf:system-source-directory :realispic))
                          (merge-pathnames "app.js"
                                           base)
                          (list :name name)))

(defun bootstrap (name &key (base nil))
  "Create the skeleton for realispic project [name] at [base]."
  (let* ((name (string-downcase name))
         (base (or base (merge-pathnames
                         (format nil "quicklisp/local-projects/~a/"
                                 name)
                         (user-homedir-pathname)))))
    (generate-asd-file name base)
    (generate-lisp-file name base)
    (generate-js-file name base)
    (let ((favico-path (merge-pathnames "assets/icon/favico.png"
                                        base)))
      (ensure-directories-exist favico-path)
      (cl-fad:copy-file (merge-pathnames "skeleton/assets/icon/peach_fur.png"
                                         (asdf:system-source-directory :realispic))
                        favico-path
                        :overwrite t))
    (format t "[ok] Project skeleton generated for [~a] at ~a."
            name
            base)))





