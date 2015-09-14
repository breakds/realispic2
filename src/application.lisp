(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.application)
    (defpackage realispic.application
      (:use :cl :parenscript)
      (:import-from :alexandria
                    :with-gensyms
                    :symbolicate)
      (:import-from :realispic.widget
                    :compile-psx
                    :*realispic-widget-table*)
      (:import-from :realispic.css
                    :compile-css)
      (:import-from :realispic.rpc
                    :rpc-middleware)
      (:export :def-app
               :*template-path*))))
(in-package :realispic.application)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun widget-dependency-closure (initial-set)
    (labels ((bfs (candidates closure)
	       (if (null candidates)
		   closure
		   (let ((new-candidates (cdr candidates))
			 (new-closure closure))
		     (loop for dep in (cdr (gethash (car candidates)
						    *realispic-widget-table*))
			unless (member dep closure :test #'string-equal) do
			  (push dep new-closure)
			  (push dep new-candidates))
		     (bfs new-candidates new-closure)))))
      (bfs initial-set initial-set)))

  (defun is-external (url)
    (let ((prefix "http"))
      (and (> (length url) (length prefix))
           (string-equal (subseq url 0 (length prefix))
                         prefix))))

  (defun match-file-type (url &rest types)
    (some (lambda (x) (string-equal (pathname-type url) x))
	  types)))

(defvar *template-path* 
  (merge-pathnames "template/realispic-template.html"
                   (asdf:system-source-directory 'realispic))
  "The path to the html template file used in realispic, shadow this
   to use customized templates.")

(defun preprocess-includes (includes static-path static-root)
  "Find all the files from the local source, and copy it to
   static-root. Quantify those files with static-path."
  (mapcar (lambda (file)
            (handler-case 
                (etypecase file
                  (string file)
                  (cons (ecase (car file)
                          (:local (let ((source (merge-pathnames
                                                 (third file)
                                                 (asdf:system-source-directory 
                                                  (second file))))
                                        (target (merge-pathnames
                                                 (third file)
                                                 static-root)))
                                    (ensure-directories-exist target)
                                    (cl-fad:copy-file source target :overwrite t)
                                    ;; Return the reconstructed static
                                    ;; path.
                                    (merge-pathnames (third file)
                                                     static-path))))))
              (t (e) (error "Bad include: ~a due to ~a" 
                            file e))))
          includes))

(defun compile-app (&key
                      (title "Undefined Name")
                      (path "/asset/")
                      (root "/tmp/")
                      (js-file-name "realispic-app.js")
                      (icon "")
                      (includes nil)
                      (widget nil))
  "Compile the javascript, css and html of the app. Return a string
   containing the generated html."
  (let ((js-file-path (merge-pathnames js-file-name root))
        (includes (preprocess-includes includes path root)))
    (ensure-directories-exist js-file-path)
    (multiple-value-bind (body referenced-widgets body-css)
        (compile-psx widget :psx-only t)
      (let ((all-dependencies (widget-dependency-closure referenced-widgets)))
            ;; Check for the existence of dependencies.
            (loop for widget-name in all-dependencies
               unless (gethash widget-name *realispic-widget-table*)
               do (error "Compile app failed: undefined widget :~a."
                         widget-name))

            ;; Write the javascript file
            (with-ps-gensyms (app-var)
              (with-open-file (output js-file-path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (write-string 
                 (ps* `(progn ,@(loop for widget-name in all-dependencies
                                   collect (funcall (car (gethash widget-name
                                                                  *realispic-widget-table*))
                                                    :javascript))
                              (let ((,app-var ,body))
                                ((@ *react render)
                                 ,app-var
                                 ((@ document get-element-by-id) "content")))))
                 output)))
            ;; Generate the html text
            (let ((html-template:*string-modifier* #'identity))
              (with-output-to-string (html-template:*default-template-output*)
                (html-template:fill-and-print-template
                 *template-path*
                 (list :title title
                       :icon icon
                       ;; Note(breakds): currently we treat everything
                       ;; included that is not javascript as css.
                       :css-include `(,@(mapcar (lambda (x) `(:url ,x))
                                                (remove-if-not (lambda (path)
                                                                 (or (match-file-type path "css")
                                                                     (not (match-file-type path "js"))))
                                                               includes)))
                       :external-js `((:url "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js")
                                      (:url "https://fb.me/react-with-addons-0.13.3.js")
                                      (:url "https://fb.me/JSXTransformer-0.13.3.js")
                                      ,@(mapcar (lambda (x) `(:url ,x))
                                                (remove-if-not (lambda (path)
                                                                 (and (match-file-type path "js")
                                                                      (is-external path)))
                                                               includes)))
                       :js-include `(,@(mapcar (lambda (x) `(:url ,x))
                                               (remove-if-not (lambda (path) 
                                                                (and (match-file-type path "js")
                                                                     (not (is-external path))))
                                                              includes)))
                       :js (format nil "~a" (merge-pathnames js-file-name path))
                       :inline-css (compile-css 
                                    (append body-css 
                                            (loop for widget-name in all-dependencies
                                               append (funcall 
                                                       (car (gethash widget-name
                                                                     *realispic-widget-table*))
                                                       :css))))))))))))

(defmacro def-app (app-name args 
                   &key
                     (server :hunchentoot)
                     (title "Undefined Name")
                     (port 5000)
                     (static-path "/assets/")
                     (static-root "/tmp/")
                     (generated-path "/main/")
                     (generated-root "/tmp/")
                     (js-file-name "realispic-app.js")
                     (icon "")
                     (includes nil)
                     (widget nil)
                     (system nil))
  ;; TODO(breakds): ensure that static-path and static-root are
  ;; directory like (tailing slash).
  (with-gensyms (env html-text)
    (let ((app-function-name (symbolicate '* app-name '-app*))
          (app-handler-name (symbolicate '* app-name '-handler '*)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; 2. Assemble the application
         (defparameter ,app-function-name
           (lack:builder 
            :session 
            (:static :path ,static-path
                     :root ,static-root)
            (:static :path ,generated-path
                     :root ,generated-root)
            #'rpc-middleware
            (let ((,html-text (compile-app :title ,title
                                           :path ,generated-path
                                           :root ,generated-root
                                           :js-file-name ,js-file-name
                                           :icon ,icon
                                           :includes ',includes
                                           :widget ',widget)))
              (lambda (,env)
                (declare (ignore ,env))
                `(200 
                  (:content-type "text/html") 
                  (,,html-text)))))
           "The global variable that holds the application function.")
         ;; 3. Create the app controller
         (defvar ,app-handler-name nil
           "The global variable that holds the application handler.")
         (defun ,app-name (&optional (command :start))
           (labels ((try-stop ()
                      (when ,app-handler-name
                        (clack:stop ,app-handler-name))))
             (ecase command
               (:start (try-stop)
                       ,@(if system
                             `((ql:quickload ,system))
                             nil)
                       (setf ,app-handler-name
                             (clack:clackup ,app-function-name
                                            :port ,port
                                            :server ,server)))
               (:stop (try-stop)))))))))
