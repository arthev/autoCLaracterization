;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :autoCLaracterization
  :description "Automatic generation of characterization tests (using 5am)"
  :version "0.1"
  :author "arthev"
  :depends-on (:fiveam     ;; ye olde test library
               :alexandria ;; contains lambda list parsers
               :closer-mop
               :bordeaux-threads
	       )
  :serial t
  :pathname "src"
  :components ((:file "packages")
               (:file "utils")
               (:file "config")
               (:file "serialization")
               (:file "test-generation")
               (:file "recorder-macros")
               (:file "hooking")
               (:file "interface")
	       ))
