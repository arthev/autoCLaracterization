;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem "autoCLaracterization-tests"
  :description "Tests for autoCLaracterization"
  :version "0.0"
  :author "arthev"
  :depends-on (:autoCLaracterization)
  :serial t
  :components ((:file "src/test/general")
               (:file "src/test/defrecfun")))
