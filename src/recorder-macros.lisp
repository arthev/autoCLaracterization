(in-package :autoCLaracterization)

(defun generate-generate-invocation-form (name required-params optional-params rest-param keyword-params)
  `(generate-invocation-form
    ',name
    ,@(when required-params
        `(:required-params
          (list ,@required-params)))
    ,@(when optional-params
        `(:optional-params
          ;; Bug: suppliedp might not exist!
          ;; Actually, can't we just normalize the lambda-list
          ;; so it's guaranteed to exist?
          `(,@,@(loop for (name _ suppliedp)
                        in optional-params
                      collect `(when ,suppliedp `(,,name))))))
    ,@(when rest-param
        `(:rest-param ,rest-param))
    ,@(when keyword-params
        `(:keyword-params
          ;; Bug: suppliedp might not exist!
          ;; Actually, can't we just normalize the lambda-list
          ;; so it's guaranteed to exist?
          `(,@,@(loop for ((keyword-name name) init suppliedp)
                        in keyword-params
                      collect `(when ,suppliedp `(,,keyword-name
                                                  ,,name))))))))

(defmacro defrecfun (name-and-options lambda-list &body body)
  "Macro to set up automatic capture of characterization tests for arbitrary functions with minimal setup/hassle.

NAME-AND-OPTIONS can be the function name, or it can be a list conforming to the below destructuring-bind on it, where OUTPUT-PATH lets you control where the tests get written to, and where TEST is the comparison test to check for pairwise same functional result. (i.e. compares primary return values using TEST, secondary return values using TEST, and so forth.) If the case is complex and you want different types of comparisons for the different return values, you can instead supply CUSTOM-TEST which must be a function of two arguments, one of which will be the list of return-values as per the expected setup in the generated test, and one of which will be the return-values list of the function call in the test itself.

LAMBDA-LIST and BODY are unsurprising - DEFRECFUN is meant to be 'dropped in' instead of DEFUN for existing functions, after all. Obviously, since DEFRECFUN tests the function as a functional interface (i.e. takes X input, gives Y output), functions that work by side-effects might not be suitable, and some functions might be more suitable after splitting up to expose hidden inner functional interfaces etc."
  (multiple-value-bind (required-params
                        optional-params
                        rest-param
                        keyword-params
                        allow-other-keys-p
                        aux-params
                        keys-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other-keys-p aux-params keys-p))
    (destructuring-bind (name &key
                                (output-path '*characterization-test-output-path*)
                                (test '#'eql)
                                (custom-test nil custom-test-supplied-p))
        (listify name-and-options)
      (with-gensyms (invocation-form return-values result-form inner-fn)
        (let ((new-body (subst inner-fn name body)))
          `(defun ,name ,lambda-list
             (let* ((,invocation-form
                      ,(generate-generate-invocation-form name
                                                          required-params
                                                          optional-params
                                                          rest-param
                                                          keyword-params))
                    (,return-values
                      (multiple-value-list
                       (labels ((,inner-fn ,lambda-list
                                  ,@new-body))
                         ,@new-body)))
                    (,result-form
                      (generate-result-form ,return-values)))
               (record-characterization-test
                ,invocation-form
                ,result-form
                ,output-path
                ,(if custom-test-supplied-p
                     custom-test
                     test))
               (values-list ,return-values))))))))

;; DEFRECMETHOD ends up more complex because even if we setup a method as recordable, things might not match in the test if the method isn't the most specific one for the given input. Maybe there's a way to shortcut so as to call the relevant method more directly? If not, maybe there's some way to instrument the whole generic function to make things work out?
