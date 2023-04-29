(defpackage lem-theme/spec
  (:use :cl)
  (:import-from #:alexandria)
  (:import-from #:lem-theme/theme
                #:theme
                #:theme-name
                #:background
                #:foreground
                #:background-mode
                #:current-theme)
  (:export #:defspec
           #:attribute-spec
           #:make-color
           #:make-attribute
           #:spec
           #:update-spec))
(in-package lem-theme/spec)


(defgeneric attribute-spec (theme attribute)
  (:method ((theme theme) (attribute symbol))
    "By default we don't provide a spec, then attribute will be ignored."
    nil)
  (:documentation "Returns a list of params for the attribute."))


(defgeneric make-color (theme value)
  (:documentation "A hook to tranform color from light to dark.")
  (:method ((theme t) value)
    value))


(defgeneric make-attribute (theme definition)
  (:documentation 
   "Replaces :fg with :foreground and :bg with :background.
    If some of these components is missing, adds defaults.")
  (:method ((theme t) (definition t))
    (loop with default-background = (background theme)
          with default-foreground = (foreground theme)
          with next-is-color = nil
          with result = nil
          with has-fg = nil
          with has-bg = nil
          for item in definition
          do (cond
               (next-is-color
                (push (make-color theme
                                  (case item
                                    ;; You can use these keywords
                                    ;; to "invert" colors
                                    (:background default-background)
                                    (:foreground default-foreground)
                                    (t item)))
                      result)
                (setf next-is-color nil))
               ((eql item :fg)
                (push :foreground
                      result)
                (setf next-is-color t
                      has-fg t))
               ((eql item :bg)
                (push :background
                      result)
                (setf next-is-color t
                      has-bg t))
               (t (push item
                        result)))
          finally (when (or has-fg has-bg)
                    (unless has-fg
                      (push :foreground
                            result)
                      (push (make-color theme default-foreground)
                            result))
                    (unless has-bg
                      (push :background
                            result)
                      (push (make-color theme default-background)
                            result)))
                  (return
                    (nreverse result)))))


(defgeneric spec (theme)
  (:method ((theme theme))
    (append (list (list :display-background-mode
                        (background-mode theme)))
            (when (foreground theme)
              (list (list :foreground
                          (make-color theme
                                      (foreground theme)))))
            (when (background theme)
              (list (list :background
                          (make-color theme
                                      (background theme)))))
            (loop for attribute in lem::*attributes*
                  for spec = (attribute-spec theme attribute)
                  when spec
                  collect (make-attribute theme
                                          (list* attribute spec)))))
  (:documentation "Returns a list of lists with a spec for all attributes for which theme defines colors."))


(defgeneric update-spec (theme)
  (:documentation "Updates theme specification in Lem's internals and reloads theme if it is enabled.")
  (:method ((theme theme))
    (setf (gethash (theme-name theme)
                   lem::*color-themes*)
          (lem::make-color-theme
           :specs (spec theme)
           :parent nil))
    (when (eql (class-of (current-theme))
               (class-of theme))
      (lem-theme/theme:reload-theme))
    (values)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-attribute-spec-method (theme spec)
    (destructuring-bind (attribute &rest a-spec)
        spec
      `(defmethod attribute-spec ((theme ,theme) (attribute (eql ',attribute)))
         (list ,@a-spec)))))


(defmacro defspec ((theme) &body specs)
  "Defines attributes for the theme"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (alexandria:curry #'make-attribute-spec-method theme)
                    specs)
          (update-spec (make-instance ',theme))))
