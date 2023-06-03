;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :autoCLaracterization
  :description "Automatic generation of characterization tests (using 5am)"
  :version "0.0"
  :author "arthev"
  :depends-on (:fiveam     ;; ye olde test library
               :alexandria ;; contains lambda list parsers
	       )
  :serial t
  :pathname "src"
  :components ((:file "packages")
               (:file "utils")
               (:file "config")
               (:file "recorder-macros")
	       ))
