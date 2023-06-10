(in-package :autoCLaracterization)

;;;; Runtime helpers

(defvar *current-recorder-depths* nil) ; nil in global space, dynamic hashtable

(defun record-according-to-strategy-p (name strategy
                                       &key (depths *current-recorder-depths*))
  (cond ((gethash :inside-outer-only depths)
         nil)
        ((eq :entry-only strategy)
         (zerop (gethash name depths 0)))
        (t ; strategy is :ALL or it's :OUTER-ONLY without being inside one yet
         t)))

(defun update-recorder-state-according-to-strategy (name strategy
                                                    &key
                                                      (depths
                                                       *current-recorder-depths*)
                                                      (value nil value-supplied-p))
  (ecase strategy
    (:all nil)
    (:outer-only (setf (gethash :inside-outer-only depths) (if value-supplied-p
                                                               value
                                                               t)))
    (:entry-only (setf (gethash name depths) (if value-supplied-p
                                                 value
                                                 (1+ (gethash name depths 0)))))))

(defun recorder-state-according-to-strategy (name strategy
                                             &key (depths
                                                   *current-recorder-depths*))
  (ecase strategy
    (:all nil)
    (:outer-only (gethash :inside-outer-only depths))
    (:entry-only (gethash name depths 0))))

;;;; Lambda-list helpers

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

;;;; Macroexpansion helpers

(defun generate-function-body (nexty-form &key test
                                            custom-test
                                            custom-test-supplied-p
                                            strategy
                                            name
                                            required-params
                                            optional-params
                                            rest-param
                                            keyword-params)
  "NEXTY-FORM is doing the 'actual function call' etc. It's a form to ,slice in."
  (assert (member strategy '(:all :outer-only :entry-only)))
  ;; Note that :outer-only takes precedence over the other two and block them.
  ;; That is, :outer-only affects _all_ recorders that happen under the
  ;; execution of the strategy, whereas :all and :entry-only are recorder-local.
  (with-gensyms (invocation-form return-values result-form
                 record-p old-recorder-state _)
    `(let* ((*current-recorder-depths*
              (or *current-recorder-depths* (make-hash-table :test #'eq)))
            (,record-p (record-according-to-strategy-p
                        ',name ,strategy
                        :depths *current-recorder-depths*))
            (,invocation-form
              (when ,record-p
                ,(generate-generate-invocation-form name
                                                    required-params
                                                    optional-params
                                                    rest-param
                                                    keyword-params)))
            (,old-recorder-state (recorder-state-according-to-strategy
                                  ',name ,strategy
                                  :depths *current-recorder-depths*)))
       (unwind-protect
            (let* ((,_ (update-recorder-state-according-to-strategy
                        ',name ,strategy
                        :depths *current-recorder-depths*))
                   (,return-values
                     (multiple-value-list
                      ,nexty-form))
                   (,result-form
                     (when ,record-p
                       (generate-result-form ,return-values))))
              (declare (ignore ,_))
              (when ,record-p
                (record-characterization-test
                 ,invocation-form
                 ,result-form
                 ,@(if custom-test-supplied-p
                       `(:custom-test ',custom-test)
                       `(:test ',test))))
              (values-list ,return-values))
         (update-recorder-state-according-to-strategy
          ',name ,strategy
          :depths *current-recorder-depths*
          :value ,old-recorder-state)))))

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

;;;; DEFRECFUN section

(defmacro defrecfun (name-and-options lambda-list &body body)
  "Macro to set up automatic capture of characterization tests for arbitrary functions with minimal setup/hassle.

NAME-AND-OPTIONS can be the function name, or it can be a list conforming to the below destructuring-bind on it, where TEST is the comparison test to check for pairwise same functional result. (i.e. compares primary return values using TEST, secondary return values using TEST, and so forth.) If the case is complex and you want different types of comparisons for the different return values, you can instead supply CUSTOM-TEST which must be a function of two arguments, one of which will be the list of return-values as per the expected setup in the generated test, and one of which will be the return-values list of the function call in the test itself. STRATEGY is one of (:all :outer-only :entry-only) where :all records all invocations, :outer-only only records the outermost invocation at any point, and :entry-only records the outermost invocation of all invoked recorders.

BODY has the same semantics as in DEFUN. LAMBDA-LIST has the same semantics as in DEFUN, except that optional and keyword parameters get normalized and fleshed out to contain suppliedp vars too (using gensyms). These changes should be invisible to the end programmer.

DEFRECFUN is meant to be 'dropped in' instead of DEFUN for existing functions. Since DEFRECFUN tests the function as a functional interface (i.e. takes X input, gives Y output), functions that work by side-effects might not be suitable, and some functions might be more suitable after splitting up to expose hidden inner functional interfaces etc."
  (destructuring-bind (name &key
                              (test '#'eql)
                              (custom-test nil custom-test-supplied-p)
                              (strategy *default-recorder-strategy*))
      (listify name-and-options)
    (let* ((lambda-list (flesh-out-lambda-list lambda-list)))
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
             `(progn ,@body)
             :test test
             :custom-test custom-test
             :custom-test-supplied-p custom-test-supplied-p
             :strategy strategy
             :name name
             :required-params required-params
             :optional-params optional-params
             :rest-param rest-param
             :keyword-params keyword-params))))))

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
      ;; The lambda list we return here will later be fed through
      ;; FLESH-OUT-LAMBDA-LIST, so normalization at this point leads to subsequent
      ;; troubles. It does end up normalized and fleshed out eventually, though.
      (alexandria:parse-ordinary-lambda-list lambda-list :normalize nil)
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

NAME-AND-OPTIONS can be the generic function name, or it can be a list conforming to the below destructuring-bind on it, where TEST is the comparison test to check for pairwise same functional result. (i.e. compares primary return values using TEST, secondary return values using TEST, and so forth.) If the case is complex and you want different types of comparisons for the different return values, you can instead supply CUSTOM-TEST which must be a function of two arguments, one of which will be the list of return-values as per the expected setup in the generated test, and one of which will be the return-values list of the call in the test itself. STRATEGY is one of (:all :outer-only :entry-only) where :all records all invocations, :outer-only only records the outermost invocation at any point, and :entry-only records the outermost invocation of all invoked recorders.

OPTIONS has the same semantics as in DEFGENERIC save that :METHOD-COMBINATION is not available (because DEFRECGENERIC fills it in automatically). LAMBDA-LIST has the same semantics as in DEFGENERIC.

DEFRECGENERIC is meant to be 'dropped in' instead of DEFGENERIC for existing generics/methods. Since DEFRECGENERIC tests the generic function as a functional interface (i.e. takes X input, gives Y output), generic functions that work by side-effects might not be suitable, and some might be more suitable after splitting up to expose hidden inner functional interfaces etc."
  (let ((entry (find :method-combination options :key #'car)))
    (when entry
      (error "DEFRECGENERIC doesn't permit option ~S" entry)))
  (destructuring-bind (name &key
                              (test '#'eql)
                              (custom-test nil custom-test-supplied-p)
                              (strategy *default-recorder-strategy*))
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
               :strategy strategy
               :name name
               :required-params required-params
               :optional-params optional-params
               :rest-param rest-param
               :keyword-params keyword-params)))))))
