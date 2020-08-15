(defpackage lem-theme
  (:use :cl)
  (:nicknames #:lem-theme/core)
  (:import-from #:lem-theme/commands
                #:theme-colors)
  (:export #:theme-colors))
(in-package lem-theme)
