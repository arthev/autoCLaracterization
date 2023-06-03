;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :autoCLaracterization
  :description "Automatic generation of characterization tests (using 5am)"
  :version "0.0"
  :author "arthev"
  :depends-on (:fiveam           ;; ye olde test library
	       )
  :serial t
  :pathname "src"
  :components ((:file "packages")
	       ))
