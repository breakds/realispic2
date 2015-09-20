(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :realispic.compiler)
    (defpackage realispic.compiler
      (:use :cl :parenscript)
      (:import-from :alexandria
                    :with-gensyms
                    :symbolicate)
      (:export :def-code-walker
               :compile-psx
               :split-argument-list
               :initial-state-slot))))
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
                      `(progn ,@,(list 'mapcar '(lambda (x) `(push (symbol-name ,x) shadowed))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *html-tags* 
    '(:html :body :head :span :header :section;; Structure
      :p :h1 :h2 :h3 :h4 :h5 :h6 :br ;; Text
      :a ;; Links
      :div :frame :iframe :form :figure :img :video ;; Container
      :aside :main :footer
      :table :tbody :tr :td :th :thead :tfoot :caption;; Table
      :nav :ul :ol :li ;; List
      :input :textarea :select :option :optgroup :button :label :fieldset :legend ;; Interaction
      :script ;; Script
      :b :i :sub :sup :big :small :hr) ;; Style
    "All the keywords that will be recognized as standard html tags by compile-psx.")

  (defmacro compile-error ((&rest format-string) &rest args)
    `(error (format nil "Realispic Compilation Error: ~{~a~}." 
                    ',format-string)
            ,@args))

  (defun split-argument-list (args)
    "Split the argument list, so that we know which of the agruments
    are atrributes (properties in React terms), and which are states."
    (let (attribute-names state-defs)
      (loop for arg in args
         do (cond ((not (listp arg))
                   (compile-error ("Expect a list for argument definition, "
                                   "but get ~a.")
                                  arg))
                  ((not (symbolp (car arg)))
                   (compile-error ("~a is defined as an argument in ~a, but "
                                   "is not a symbol.")
                                  (car arg) arg))
                  ((eq (cadr arg) :attribute)
                   (push (symbol-name (car arg)) 
                         attribute-names))
                  ((eq (cadr arg) :state)
                   (push (list (car arg) (third arg))
                         state-defs))
                  (t (compile-error ("~a is not a recognizable label of arguments. "
                                     "Valid labels are :atrribute and :state.")
                                    (cadr arg)))))
      (values attribute-names state-defs)))

  (defun initial-state-slot (state-defs)
    "Return the parenscript code that defines the get-initial-state
    slot in React's create-class input."
    `(lambda ()
       (create ,@(mapcan #'identity state-defs))))

  (defun one-of-symbols-p (input-form symbol-names)
    (and (symbolp input-form)
         (member (symbol-name input-form) symbol-names
                 :test #'string-equal)))

  (defun unquantify-keyword (keyword)
    "Convert a keyword into its counter-part without the leading ':'."
    (symbolicate (string keyword)))

  (defun process-style-name (style-name)
    (alexandria:symbolicate style-name))

  (defun compile-animation (pairs)
    (let* ((animation-name (format nil "tween-~a" (ps-gensym)))
           keyframes animation-block)
           ;; (keyframes `(,animation-name :keyframes))
           ;; (animation-block `((,(format nil ".~a" animation-name)))))
      (loop for (key value) on pairs by #'cddr do
           (case key
             (:keyframe (push (cons (list (car value)) (cdr value))
                              keyframes))
             ;; otherwise, must be animation- prefixed.
             (t (let ((*print-case* :downcase))
                  (push value animation-block)
                  (push (format nil "animation-~a" key) animation-block)))))
      (values (list (list* :keyframes animation-name keyframes)
                    (list* `(,(format nil ".~a" animation-name)) 
                           "animation-name"
                           animation-name
                           animation-block))
              animation-name))))

(def-code-walker compile-psx (attribute-names state-defs dependencies psx-only css)
    ((atom-attribute () (when (and (atom form)
				   (one-of-symbols-p form attribute-names)
                                   (not (one-of-symbols-p form shadowed)))
                          `(@ this props ,form)))
     (chain-ref ((at-symbol :symbol "@") &rest paths)
                `(@ ,(process (car paths)) 
                     ,@(loop for path in (cdr paths)
                          collect (if (atom path)
                                      path
                                      (process path
                                               :off `(,#'atom-attribute))))))
     (let-form ((let-symbol :symbol "let") bindings &rest body)
               `(let ,(mapcar (lambda (binding)
                                (list (car binding)
                                      (process (second binding))))
                              bindings)
		  ,@(progn (mapcar (lambda (binding) (push-shadowed (car binding))) 
				   bindings)
			   nil)
                  ,@(process-each body)))
     (let*-form ((let-symbol :symbol "let*") bindings &rest body)
		`(let* ,(mapcar (lambda (binding)
				  (let ((result (list (car binding)
						      (process (second binding)))))
				    (push-shadowed (car binding))
				    result))
				bindings)
		   ,@(process-each body)))
     (lambda-form ((lambda-symbol :symbol "lambda") arg-list &rest body)
		  `(lambda ,(mapcar (lambda (arg)
				      ;; TODO(breakds): Should
				      ;; consider shadow in default
				      ;; value forms.
				      (cond ((match-&-symbol arg) arg)
					    ((match-symbol arg) 
					     (push-shadowed arg)
					     arg)
					    ((and (listp arg) (match-symbol (car arg)))
					     (push-shadowed (car arg))
					     arg)
					    (t (error "lambda-form: ~a is not a valid argument."
						      arg))))
				    arg-list)
		     ,@(process-each body)))
     (update-state-form ((fun-name :symbol "update-state") &rest pairs)
                        (let ((valid-state-names (mapcar (lambda (x)
                                                           (symbol-name (car x)))
                                                         state-defs)))
                          `((@ this set-state) 
                            (create ,@(loop for (var-name value) 
                                         on pairs by #'cddr
                                         append (if (member (symbol-name var-name)
                                                            valid-state-names
                                                            :test #'string-equal)
                                                    (list var-name 
                                                          (process value
                                                                   :off `(,#'update-state-form
                                                                          ,#'psx-tags)))
                                                    (error (concatenate 'string
                                                                        "state-ref: ~a is not a valid "
                                                                        "state name. Expect one of "
                                                                        "{~{~a~^, ~}}.")
                                                           var-name valid-state-names)))))))
     (child-ref ((child-key :keyword) &rest indices)
		(when (eq child-key :children)
		  (case (length indices)
		    (0 `(@ this props children))
		    (1 `(aref (@ this props children) ,(process (car indices)
                                                                :off `(,#'psx-tags))))
		    (t `(list ,@(mapcar (lambda (x)
                                          `(aref (@ this props children)
                                                 ,(process x
                                                           :off `(,@'pxs-tags))))
                                        indices))))))
     (state-ref ((state-key :keyword) var-name)
		(when (eq state-key :state)
		  (unless (match-symbol var-name)
		    (error "state-ref: expect a symbol as state name but get ~a."
			   var-name))
		  (let ((valid-state-names (mapcar (lambda (x) (symbol-name (car x)))
						   state-defs)))
		    (unless (member (symbol-name var-name)
				    valid-state-names
				    :test #'string-equal)
		      (error (concatenate 'string
                                          "state-ref: ~a is not a valid state name. "
                                          "Expect one of {~{~a~^, ~}}.")
			     var-name valid-state-names)))
		  `(@ this state ,var-name)))
     (local-slots ((operator :symbol "with-slots") slots &rest body)
                  `(with-slots ,slots
                       ,@(process-each body)))
     (psx-tags ((tag :keyword) attributes &rest body)
	       (unless (or (eq tag :state)
                           (eq tag :children))
		 ;; Keyword case, can be either a standard html tag, or
		 ;; a custom tag.  Call React.DOM.tag-name when it is a
		 ;; standard html-tag, or the custom ReactClass
		 ;; constructor otherwise.  TODO(breakds): Compile time
		 ;; error if tag is not recognizable.
		 (unless (listp attributes)
		   ;; Validation of attributes.
		   (error "psx-tags: expect list as attributes for ~a but get ~a."
			  tag
			  attributes))

                 (let (classes)
                   ;; First pass of attributes scan. Calculate anything
                   ;; that needs to be done before generating the actual
                   ;; compiled code.
                   (loop for entry in attributes
                      when (string-equal (car entry) "animation")
                      do (multiple-value-bind (css-result animation-class)
                             (compile-animation (cdr entry))
                           (loop for rule in css-result
                              do (push rule css))
                           (push animation-class classes)))
                   ;; Second pass of attributes scan. Emit code.
                   `(,@(cond ((member tag *html-tags*)
                              `((@ *react *dom* ,(unquantify-keyword tag))))
                             ((eq tag :transition)
                              `((@ *react create-element) (@ *react addons *c-s-s-transition-group)))
                             (t (let ((widget-name (unquantify-keyword tag)))
                                  ;; Mark the widget as a denpendency of
                                  ;; the widget being defined
                                  (pushnew (symbol-name widget-name) dependencies
                                           :test #'string-equal)
                                  `((@ *react create-element) ,widget-name))))
                       ;; Handle input-attributes provided for this tag.
                       ;; Note that we DO NOT allow for PSX syntax in
                       ;; attributes.
                       ;;
                       ;; This is understandable because we never put
                       ;; html code inside html attributes.
                       (create ,@(mapcan 
                                  (lambda (attribute-pair)
                                    (cond ((string-equal (car attribute-pair) "class")
                                           (loop for entry in (cdr attribute-pair)
                                              do (push entry classes)))
                                          (t (list (car attribute-pair)
                                                   (cond ((string-equal (car attribute-pair)
                                                                        "style")
                                                          `(create ,@(loop for (style-name style-value)
                                                                        on (rest attribute-pair)
                                                                        by #'cddr
                                                                        append (list (process-style-name 
                                                                                      style-name)
                                                                                     (process style-value)))))
                                                         (t (process (cadr attribute-pair) 
                                                                     :off `(,#'psx-tags))))))))
                                  attributes)
                               ;; put class in
                               ,@(when classes
                                       `(class-name
                                         (+ ,@(mapcan (lambda (x)
                                                        `(" " ,(process x :off `(,#'psx-tags))))
                                                      classes)))))
                       ,@(process-each body)))))
     (top-level () (when (or (atom form)
			     (not (match-symbol (car form) "labels")))
		     `(render (lambda () ,(process form
						   :off (list #'top-level 
							      #'top-level-labels)
						   :on (list #'atom-attribute
                                                             #'chain-ref
							     #'let-form
							     #'let*-form
							     #'lambda-form
							     #'psx-tags
                                                             #'update-state-form
							     #'state-ref
                                                             #'child-ref
                                                             #'local-slots))))))
     (top-level-labels ((labels-symbol :symbol "labels") fun-defs &rest body)
		       `(render 
			 (lambda ()
			   ,@(process-each body 
					   :off (list #'top-level #'top-level-labels)
					   :on (list #'atom-attribute
                                                     #'chain-ref
						     #'let-form
						     #'let*-form
						     #'lambda-form
						     #'psx-tags
                                                     #'update-state-form
						     #'state-ref
                                                     #'child-ref
                                                     #'local-slots)))
			 ,@(mapcan (lambda (fun-def)
				     (list (car fun-def)
					   (process (cons 'lambda (rest fun-def)) 
						    :off (list #'top-level #'top-level-labels)
						    :on (list #'atom-attribute
                                                              #'chain-ref
							      #'let-form
							      #'let*-form
							      #'lambda-form
							      #'psx-tags
                                                              #'update-state-form
							      #'state-ref
                                                              #'child-ref
                                                              #'local-slots))))
				   fun-defs))))
  (if psx-only 
      (values (initialize #'psx-tags) dependencies css)
      (values (initialize #'top-level #'top-level-labels) dependencies css)))
