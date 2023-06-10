(defpackage :autoCLaracterization
  (:use :cl)
  (:export :defrecfun
           :defrecgeneric
           :*characterization-test-output-path*
           :*default-recorder-strategy*
           :write-characterization-tests))
