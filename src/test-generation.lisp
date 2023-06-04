(in-package :autoCLaracterization)

(defvar *recorder-lock* (bt:make-lock "recorder-lock"))

(defvar *characterization-tests* (make-hash-table))

(defun record-characterization-test (invocation-form
                                     result-form
                                     &key test custom-test)
  (let* ((fn-name (car invocation-form))
         (test-form
           (generate-characterization-test-case invocation-form
                                                result-form
                                                :test test
                                                :custom-test custom-test)))
    (bt:with-lock-held (*recorder-lock*)
      (pushnew test-form
               (gethash fn-name *characterization-tests*)
               :test #'equal))))

(defun generate-characterization-test-case (invocation-form
                                            result-form
                                            &key test custom-test)
  "TEST and CUSTOM-TEST correspond to the description for DEFRECFUN, namely
that TEST is a comparison that gets applied pairwise to elements in the value-lists, whereas CUSTOM-TEST (if supplied) 1) takes precedence 2) applies to the value-lists themselves."
  (if custom-test
      `(5am:is (funcall ,custom-test
                        ,result-form
                        (multiple-value-list
                         ,invocation-form)))
      `(5am:is (every ,test
                      ,result-form
                      (multiple-value-list
                       ,invocation-form)))))

(defun generate-characterization-test (fn-name test-case-forms)
  `(5am:test ,(keywordify (format nil "~A:~A"
                                  fn-name
                                  (get-universal-time)))
     ,@test-case-forms))
