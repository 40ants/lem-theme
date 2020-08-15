(defpackage lem-theme/theme
  (:use :cl)
  (:export #:theme
           #:background-mode
           #:background
           #:foreground
           #:theme-name
           #:current-theme
           #:all-colors
           #:on-load
           #:on-unload
           #:load-theme
           #:reload-theme))
(in-package lem-theme/theme)

(defvar *current-theme* nil)


(defclass theme ()
  ((background-mode :initform :dark
                    :reader background-mode)
   (background :initform nil
               :reader background)
   (foreground :initform nil
               :reader foreground)))


(defgeneric theme-name (theme)
  (:method ((theme theme))
    (string-downcase
     (class-name
      (class-of theme)))))


(defgeneric all-colors (theme)
  (:documentation "Should return a list of symbol, where each symbol is bound to the color used in the theme.")
  (:method ((theme theme))
    nil))


(defun current-theme ()
  *current-theme*)


(defgeneric on-load (theme)
  (:method ((theme theme))
    (values)))

(defgeneric on-unload (theme)
  (:method ((theme theme))
    (values)))

(defun load-theme (theme-class)
  (unless (symbolp theme-class)
    (error "Pass a symbol to the `load-theme'."))
  
  (unless (find-class theme-class)
    (error "Unable to find a theme class ~S." theme-class))
  
  (when *current-theme*
    (on-unload *current-theme*))
  
  (setf *current-theme*
        (make-instance theme-class))
  
  (on-load *current-theme*)
  (lem:load-theme (theme-name *current-theme*)))


(defun reload-theme ()
  (when *current-theme*
    (on-unload *current-theme*)
    (on-load *current-theme*)
    (lem:load-theme (theme-name *current-theme*))))
