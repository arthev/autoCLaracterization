;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :autoCLaracterization-tests
  :description "Tests for autoCLaracterization"
  :version "0.0"
  :author "arthev"
  :depends-on (:autoCLaracterization
               :fiveam)
  :serial t
  :components ((:file "src/test/general")
               (:file "src/test/defrecfun")
               (:file "src/test/defrecgeneric")
               (:file "src/test/defrecmethod")
               (:file "src/test/hooking")))
