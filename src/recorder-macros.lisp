(in-package :autoCLaracterization)

(defun generate-function-body (nexty-form &key test
                                            custom-test
                                            custom-test-supplied-p
                                            name
                                            required-params
                                            optional-params
                                            rest-param
                                            keyword-params)
  "NEXTY-FORM is doing the 'actual function call' etc. It's a form to ,slice in."
  (with-gensyms (invocation-form return-values result-form)
    `(let* ((,invocation-form
              ,(generate-generate-invocation-form name
                                                  required-params
                                                  optional-params
                                                  rest-param
                                                  keyword-params))
            (,return-values
              (multiple-value-list
               ,nexty-form))
            (,result-form
              (generate-result-form ,return-values)))
       (record-characterization-test
        ,invocation-form
        ,result-form
        ,@(if custom-test-supplied-p
              `(:custom-test ',custom-test)
              `(:test ',test)))
       (values-list ,return-values))))

(defun generate-generate-invocation-form (name required-params optional-params rest-param keyword-params)
  (assert (fleshed-out-optional-params-p optional-params))
  (assert (fleshed-out-keyword-params-p keyword-params))
  ;; Given the above two asserts, we now know all optionals/keywords
  ;; e.g. have supplied-p bindings.
  `(generate-invocation-form
    ',name
    ,@(when required-params
        `(:required-params
          (list ,@required-params)))
    ,@(when optional-params
        `(:optional-params
          `(,@,@(loop for (name _ suppliedp)
                        in optional-params
                      collect `(when ,suppliedp `(,,name))))))
    ,@(when rest-param
        `(:rest-param ,rest-param))
    ,@(when keyword-params
        `(:keyword-params
          `(,@,@(loop for ((keyword-name name) init suppliedp)
                        in keyword-params
                      collect `(when ,suppliedp `(,,keyword-name
                                                  ,,name))))))))

(defun fleshed-out-optional-params-p (params)
  (every (lambda (param)
           (destructuring-bind (name init suppliedp) param
             (declare (ignore name init))
             suppliedp))
         params))

(defun fleshed-out-keyword-params-p (params)
  (every (lambda (param)
           (destructuring-bind ((keyword-name name) init suppliedp) param
             (declare (ignore keyword-name name init))
             suppliedp))
         params))

(defun flesh-out-normalized-optional-params (params)
  (mapcar (lambda (param)
            (destructuring-bind (name init suppliedp) param
              `(,name ,init ,(if suppliedp
                                 suppliedp
                                 (gensym (symbol-name name))))))
          params))

(defun flesh-out-normalized-keyword-params (params)
  (mapcar (lambda (param)
            (destructuring-bind ((keyword-name name) init suppliedp) param
              `((,keyword-name ,name) ,init ,(if suppliedp
                                                 suppliedp
                                                 (gensym (symbol-name name))))))
          params))

(defun make-ordinary-lambda-list (required-params
                                  optional-params
                                  rest-param
                                  keyword-params
                                  allow-other-keys-p
                                  aux-params
                                  keys-p)
  `(,@required-params
    ,@(when optional-params
        `(&optional ,@optional-params))
    ,@(when rest-param
        `(&rest ,rest-param))
    ,@(when keys-p
        `(&key ,@keyword-params ,@(when allow-other-keys-p
                                    `(&allow-other-keys))))
    ,@(when aux-params
        `(&aux ,@aux-params))))

(defun flesh-out-lambda-list (lambda-list)
  (multiple-value-bind (required-params
                        optional-params
                        rest-param
                        keyword-params
                        allow-other-keys-p
                        aux-params
                        keys-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let* ((optional-params (flesh-out-normalized-optional-params
                             optional-params))
           (keyword-params (flesh-out-normalized-keyword-params
                            keyword-params)))
      (make-ordinary-lambda-list required-params
                                 optional-params
                                 rest-param
                                 keyword-params
                                 allow-other-keys-p
                                 aux-params
                                 keys-p))))

;;;; DEFRECFUN section

(defmacro defrecfun (name-and-options lambda-list &body body)
  "Macro to set up automatic capture of characterization tests for arbitrary functions with minimal setup/hassle.

NAME-AND-OPTIONS can be the function name, or it can be a list conforming to the below destructuring-bind on it, where TEST is the comparison test to check for pairwise same functional result. (i.e. compares primary return values using TEST, secondary return values using TEST, and so forth.) If the case is complex and you want different types of comparisons for the different return values, you can instead supply CUSTOM-TEST which must be a function of two arguments, one of which will be the list of return-values as per the expected setup in the generated test, and one of which will be the return-values list of the function call in the test itself.

BODY has the same semantics as in DEFUN. LAMBDA-LIST has the same semantics as in DEFUN, except that optional and keyword parameters get normalized and fleshed out to contain suppliedp vars too (using gensyms). These changes should be invisible to the end programmer.

DEFRECFUN is meant to be 'dropped in' instead of DEFUN for existing functions. Since DEFRECFUN tests the function as a functional interface (i.e. takes X input, gives Y output), functions that work by side-effects might not be suitable, and some functions might be more suitable after splitting up to expose hidden inner functional interfaces etc."
  (destructuring-bind (name &key
                              (test '#'eql)
                              (custom-test nil custom-test-supplied-p))
      (listify name-and-options)
    (with-gensyms (inner-fn)
      (let* ((new-body (subst inner-fn name body))
             (lambda-list (flesh-out-lambda-list lambda-list)))
        (multiple-value-bind (required-params
                              optional-params
                              rest-param
                              keyword-params
                              allow-other-keys-p
                              aux-params
                              keys-p)
            (alexandria:parse-ordinary-lambda-list lambda-list)
          (declare (ignore allow-other-keys-p aux-params keys-p))
          `(defun ,name ,lambda-list
             ,(generate-function-body
               `(labels ((,inner-fn ,lambda-list
                           ,@new-body))
                  ,@new-body)
               :test test
               :custom-test custom-test
               :custom-test-supplied-p custom-test-supplied-p
               :name name
               :required-params required-params
               :optional-params optional-params
               :rest-param rest-param
               :keyword-params keyword-params)))))))

;;;; DEFRECGENERIC section

(defun multiple-superarounds-error-string-p (e)
  (search "Only 1 :superaround permitted per generic" (format nil "~A" e)))

(deftype multiple-superarounds-error ()
  `(and error
        (satisfies multiple-superarounds-error-string-p)))

(define-method-combination superstandard ()
        ((around (:around))
         (superaround (:superaround))
         (before (:before))
         (primary () :required t)
         (after (:after) :order :most-specific-last))
  "SUPERSTANDARD method combination to wrap a single :superaround around an
otherwise standard method combination using generic. Hence instrumentive behaviour
can be added without modifying any existing DEFMETHOD forms. Balks if multiple
:superaround exist for a given generic function."
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods after))
                    `(call-method ,(first primary)))))
      (cond (superaround
             (if (< 1 (length superaround))
                 (invalid-method-error
                  (car superaround)
                  "Only 1 :superaround permitted per generic, but found: ~S"
                  superaround)
                 `(call-method ,(first superaround)
                               (,@around
                                (make-method ,form)))))
            (around
             `(call-method ,(first around)
                           (,@(rest around)
                            (make-method ,form))))
            (t
             form)))))

(defun generate-defrecgeneric-method-lambda-list (lambda-list)
  "Necessary since e.g. a generic might have (&key &allow-other-keys)
without a &rest for example."
  (multiple-value-bind (required-params
                        optional-params
                        rest-param
                        keyword-params
                        allow-other-keys-p
                        aux-params
                        keys-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let ((congruent-lambda-list
            (if (or rest-param (not keys-p))
                lambda-list
                (make-ordinary-lambda-list required-params
                                           optional-params
                                           (gensym "&REST")
                                           keyword-params
                                           allow-other-keys-p
                                           aux-params
                                           keys-p))))
      congruent-lambda-list)))

(defmacro defrecgeneric (name-and-options lambda-list &body options)
  "Macro to set up automatic capture of characterization tests for arbitrary generic functions with minimal setup/hassle.

NAME-AND-OPTIONS can be the generic function name, or it can be a list conforming to the below destructuring-bind on it, where TEST is the comparison test to check for pairwise same functional result. (i.e. compares primary return values using TEST, secondary return values using TEST, and so forth.) If the case is complex and you want different types of comparisons for the different return values, you can instead supply CUSTOM-TEST which must be a function of two arguments, one of which will be the list of return-values as per the expected setup in the generated test, and one of which will be the return-values list of the call in the test itself.

OPTIONS has the same semantics as in DEFGENERIC save that :METHOD-COMBINATION is not available (because DEFRECGENERIC fills it in automatically). LAMBDA-LIST has the same semantics as in DEFGENERIC.

DEFRECGENERIC is meant to be 'dropped in' instead of DEFGENERIC for existing generics/methods. Since DEFRECGENERIC tests the generic function as a functional interface (i.e. takes X input, gives Y output), generic functions that work by side-effects might not be suitable, and some might be more suitable after splitting up to expose hidden inner functional interfaces etc."
  (let ((entry (find :method-combination options :key #'car)))
    (when entry
      (error "DEFRECGENERIC doesn't permit option ~S" entry)))
  (destructuring-bind (name &key
                              (test '#'eql)
                              (custom-test nil custom-test-supplied-p))
      (listify name-and-options)
    (let ((method-lambda-list (flesh-out-lambda-list
                               (generate-defrecgeneric-method-lambda-list
                                lambda-list))))
      (multiple-value-bind (required-params
                            optional-params
                            rest-param
                            keyword-params
                            allow-other-keys-p
                            aux-params
                            keys-p)
          (alexandria:parse-ordinary-lambda-list method-lambda-list)
        (declare (ignore allow-other-keys-p aux-params keys-p))
        `(progn
           (defgeneric ,name ,lambda-list
             ,@options
             (:method-combination superstandard))
           (defmethod ,name :superaround ,method-lambda-list
             ,(generate-function-body
               `(call-next-method)
               :test test
               :custom-test custom-test
               :custom-test-supplied-p custom-test-supplied-p
               :name name
               :required-params required-params
               :optional-params optional-params
               :rest-param rest-param
               :keyword-params keyword-params)))))))



;; TODO: Observe that DEFRECGENERIC doesn't contain itself to recording only
;; the outermost function call. We can avoid this by using a dynamic, standard
;; strategy. But if we do that, we might as well use the same strategy for
;; DEFRECFUN and save the whole LABELS part.
;; There's essentially three main approaches:
;; 1) record all fn calls
;; 2) record outer fn call only (i.e. first encountered recfun)
;; 3) record outer fn call only per fn
;; I'm not yet committed to either idea, but it's clearly a design question.
;; Though, perhaps, I weakly prefer approach 3.
