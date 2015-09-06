(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.compiler)
    (defpackage realispic.compiler
      (:use :cl :parenscript)
      (:import-from :alexandria
                    :with-gensyms)
      (:export :def-code-walker))))
(in-package :realispic.compiler)

;; TODO(breakds): compiler utils should live in antoher file,
;; e.g. compiler-utils.lisp.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun match-symbol (input-form &optional (symbol-name nil))
    (and (symbolp input-form)
         (not (keywordp input-form))
         (if symbol-name
             (string-equal (symbol-name input-form) symbol-name)
             t)))

  (defun match-&-symbol (input-form &optional (symbol-name nil))
    (and (symbolp input-form)
         (not (keywordp input-form))
         (eq (char (symbol-name input-form) 0) #\&)
         (if symbol-name
             (string-equal (symbol-name input-form) symbol-name)
             t)))

  ;; Example Matchers:
  ;; 1) ((:symbol "lambda") args &rest body)
  ;; 2) ((:symbol "let") bindings &rest body)
  ;; 3) ((:keyword "html") attributes 
  (defun compile-matcher (name pattern body)
    (with-gensyms (shadowed)
      `(,name (form ,shadowed matcher-list)
	      (declare (ignorable form matcher-list))
              ;; Destructring bind the input form into the pattern.
              (let ((shadowed ,shadowed))
		(declare (ignorable shadowed))
                ,(if pattern
                     (let ((lambda-list 
                            (loop for entry in pattern
                               collect (cond ((match-&-symbol entry "&rest")
                                              entry)
                                             ((and (match-symbol entry)
                                                   (not (match-&-symbol entry)))
                                              entry)
                                             ((and (listp entry)
                                                   (match-symbol (car entry))
                                                   (not (match-&-symbol (car entry))))
                                              (car entry))
                                             (t (error "compile-matcher: failed to parse pattern ~a."
                                                       pattern))))))
                       `(and 
                         ;; first try destructuring-bind, if it fails, it
                         ;; suggests "no match".
                         (handler-case 
                             (destructuring-bind ,lambda-list form
                               (declare (ignore ,@(remove-if (lambda (x) (match-&-symbol x))
                                                             lambda-list)))
                               t)
                           (t () nil))
                         ;; If pass the destructuring-bind test, safely
                         ;; proceed.
                         (destructuring-bind ,lambda-list form
                           ;; Evaluate the condition enforced by the pattern
                           (when (and ,@(loop for entry in pattern
                                           when (listp entry)
                                           collect (cond ((eq (second entry) :symbol)
                                                          ;; dicatate the bound value
                                                          ;; is a symbol, or is a
                                                          ;; symbol with the specified name
                                                          (case (length entry)
                                                            (2 `(match-symbol ,(car entry)))
                                                            (3 `(match-symbol ,(car entry) ,(third entry)))
                                                            (t (error (concatenate 'string 
                                                                                   "compile-matcher: "
                                                                                   "Too many specifiers in ~a.")
                                                                      entry))))
                                                         ((eq (second entry) :keyword)
                                                          (if (= (length entry) 2)
                                                              `(keywordp ,(car entry))
                                                              (error (concatenate 'string
                                                                                  "compile-matcher: "
                                                                                  "Too many specifiers in ~a.")
                                                                     entry)))
                                                         (t (error (concatenate 'string
                                                                                "compile-matcher: "
                                                                                "Invalid entry ~a.")
                                                                   entry)))))
                             ,@body))))
                     `(progn ,@body)))))))

(defmacro def-code-walker (name extra-args (&rest matchers) &body body)
  (with-gensyms (form shadowed sub-form enabled-matchers matcher result args updated-matchers)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name (form &key ,@extra-args)
	 (macrolet ((process (,form &rest ,args)
		      `(execute ,,form shadowed matcher-list ,@,args))
		    (process-each (,form &rest ,args)
		      `(loop for ,',sub-form in ,,form
			  collect (execute ,',sub-form shadowed matcher-list ,@,args)))
		    (push-shadowed (&rest ,args)
		      `(progn ,@,(list 'mapcar '(lambda (x) (push (symbol-name x) shadowed))
				       args))))
	   (labels ((execute (,form ,shadowed ,enabled-matchers &key (on nil) (off nil))
		      (let ((,updated-matchers (remove-if (lambda (x) (member x off))
							  (union ,enabled-matchers on))))
			(or (loop 
			       for ,matcher in ,updated-matchers
			       for ,result = (funcall ,matcher ,form ,shadowed ,updated-matchers)
			       when ,result
			       return ,result)
                            ;; If nothing matches, fall back to the
                            ;; default cases.
			    (and (atom ,form) ,form)
			    (loop for ,sub-form in ,form
			       collect (execute ,sub-form ,shadowed ,updated-matchers)))))
		    (initialize (&rest ,enabled-matchers)
		      (funcall #'execute form nil ,enabled-matchers))
		    ,@(mapcar (lambda (x)
                                (compile-matcher (car x) (second x) (cddr x)))
			      matchers))
	     ,@body))))))
