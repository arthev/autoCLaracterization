(defpackage :autoCLaracterization
  (:use :cl)
  (:export :defrecfun
           :defrecgeneric
           :defrecmethod
           :*characterization-test-output-path*
           :*default-recorder-strategy*
           :*record-conditions*
           :register-recorder
           :*characterization-tests*
           :write-characterization-tests
           :turn-off-characterizing
           :turn-on-characterizing))
