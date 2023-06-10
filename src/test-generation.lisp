(in-package :autoCLaracterization)

(defvar *recorder-lock* (bt:make-lock "recorder-lock"))

(defvar *characterization-tests* (make-hash-table))

(defun record-test (invocation-form test-form)
  (let ((fn-name (car invocation-form)))
    (bt:with-lock-held (*recorder-lock*)
      (pushnew test-form
               (gethash fn-name *characterization-tests*)
               :test #'equal))))

(defun record-characterization-test (invocation-form
                                     result-form
                                     &key test custom-test)
  "TEST and CUSTOM-TEST correspond to the description for DEFRECFUN, namely
that TEST is a comparison that gets applied pairwise to elements in the value-lists, whereas CUSTOM-TEST (if supplied) 1) takes precedence 2) applies to the value-lists themselves."
  (record-test
   invocation-form
   (if custom-test
       `(5am:is (funcall ,custom-test
                         ,result-form
                         (multiple-value-list
                          ,invocation-form)))
       `(5am:is (every ,test
                       ,result-form
                       (multiple-value-list
                        ,invocation-form))))))

(defun record-condition-characterization-test (invocation-form
                                               condition)
  ;; This is slightly simplistic - more detail's possible by looking at e.g.
  ;; the string representation and such. But this suffices, I think.
  (record-test
   invocation-form
   `(5am:signals ,(type-of condition)
      ,invocation-form)))

(defun generate-characterization-test (fn-name test-case-forms)
  `(5am:test ,(keywordify (format nil "~A:~A"
                                  fn-name
                                  (get-universal-time)))
     ,@test-case-forms))
