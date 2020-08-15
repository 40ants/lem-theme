(defpackage lem-theme/commands
  (:use :cl)
  (:import-from #:lem-theme/spec)
  (:import-from #:lem-theme/theme
                #:all-colors
                #:current-theme
                #:theme-name)
  (:export #:theme-colors))
(in-package lem-theme/commands)


(lem:define-command theme-colors () ()
  "Draws a theme's palette in a separate buffer.
   Useful to check if all colors are distinguishable from
   each other."
  (let* ((buffer (lem:make-buffer "*colors*"))
         (max-length
           (loop for color in (all-colors (current-theme))
                 for name = (symbol-name color)
                 maximizing (length name)))
         (left-format (format nil " ~~~A@A " max-length))
         (right-format (format nil " ~~~AA " max-length)))
    
    (setf (lem:buffer-read-only-p buffer)
          nil)
    
    (lem:switch-to-buffer buffer)
    (lem:erase-buffer)
    
    (flet ((newline ()
             (lem:insert-character
              (lem:current-point)
              #\Newline))
           (ins-string (text &optional attribute)
             (lem:insert-string
              (lem:current-point)
              text
              :attribute attribute))
           (full-symbol-name (symbol)
             (format nil "~S" symbol)))

      (when (current-theme)
        (let ((theme (current-theme)))
          (ins-string (format nil "Theme ~A"
                              (theme-name theme)))
          (newline)
          (newline)
          
          (ins-string "Color palette:")
          (newline)
          (newline)

          (loop for color in (all-colors (current-theme))
                for value = (symbol-value color)
                for name = (symbol-name color)
                do (ins-string
                    (format nil left-format name)
                    (lem:make-attribute :background :black
                                        :foreground value))
                   (ins-string
                    (format nil right-format name)
                    (lem:make-attribute :background value
                                        :foreground :black))
                   (newline))
          
          (flet ((has-spec-p (attribute)
                   (lem-theme/spec::attribute-spec theme attribute)))
            (let* ((all-attributes (sort (copy-list lem::*attributes*)
                                         #'string>
                                         :key #'full-symbol-name))
                   (with-spec (remove-if-not #'has-spec-p all-attributes))
                   (without-spec (remove-if #'has-spec-p all-attributes)))
          
              (when with-spec
                (newline)
                (ins-string "Attributes having color spec:")
                (newline)
                (newline)
    
                (loop for attribute in with-spec
                      do (ins-string
                          (full-symbol-name attribute)
                          attribute)
                         (newline)))

              (when without-spec
                (newline)
                (ins-string "Attributes without spec:")
                (newline)
                (newline)
                (loop for attribute in without-spec
                      for symbol-name = (full-symbol-name attribute)
                      do (ins-string symbol-name
                                     attribute)
                         ;; Probably there is a problem with attribute's color
                         ;; to make it's name readable, we'll duplicate it in a raw.
                         (ins-string " - ")
                         (ins-string symbol-name)
                         (newline)))))))
          
      (setf (lem:buffer-read-only-p buffer)
            t))))