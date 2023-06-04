(in-package :autoCLaracterization)

(defun write-characterization-tests (&optional
                                       (output-path
                                        *characterization-test-output-path*))
  "Writes collected characterization tests to OUTPUT-PATH.

Collected characterization tests are the ones
currently in *CHARACTERIZATION-TESTS*."
  (bt:with-lock-held (*recorder-lock*)
    (with-open-file (out
                     output-path
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
      (maphash (lambda (fn-name test-case-forms)
                 (format out
                         "~S~%~%"
                         (generate-characterization-test
                          fn-name
                          (reverse test-case-forms))))
               *characterization-tests*)
      (finish-output out)
      (clrhash *characterization-tests*)))
  t)
