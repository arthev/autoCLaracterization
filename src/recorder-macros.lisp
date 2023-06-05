(in-package :autoCLaracterization)

;;;; DEFRECFUN section

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

(defmacro defrecfun (name-and-options lambda-list &body body)
  "Macro to set up automatic capture of characterization tests for arbitrary functions with minimal setup/hassle.

NAME-AND-OPTIONS can be the function name, or it can be a list conforming to the below destructuring-bind on it, where TEST is the comparison test to check for pairwise same functional result. (i.e. compares primary return values using TEST, secondary return values using TEST, and so forth.) If the case is complex and you want different types of comparisons for the different return values, you can instead supply CUSTOM-TEST which must be a function of two arguments, one of which will be the list of return-values as per the expected setup in the generated test, and one of which will be the return-values list of the function call in the test itself.

BODY has the same semantics as in DEFUN. LAMBDA-LIST has the same semantics as in DEFUN, except that optional and keyword parameters get normalized and fleshed out to contain suppliedp vars too (using gensyms). These changes should be invisible to the end programmer.

DEFRECFUN is meant to be 'dropped in' instead of DEFUN for existing functions. Since DEFRECFUN tests the function as a functional interface (i.e. takes X input, gives Y output), functions that work by side-effects might not be suitable, and some functions might be more suitable after splitting up to expose hidden inner functional interfaces etc."
  (multiple-value-bind (required-params
                        optional-params
                        rest-param
                        keyword-params
                        allow-other-keys-p
                        aux-params
                        keys-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (destructuring-bind (name &key
                                (test '#'eql)
                                (custom-test nil custom-test-supplied-p))
        (listify name-and-options)
      (with-gensyms (invocation-form return-values result-form inner-fn)
        (let* ((new-body (subst inner-fn name body))
               (optional-params (flesh-out-normalized-optional-params optional-params))
               (keyword-params (flesh-out-normalized-keyword-params keyword-params))
               (lambda-list (make-ordinary-lambda-list required-params
                                                       optional-params
                                                       rest-param
                                                       keyword-params
                                                       allow-other-keys-p
                                                       aux-params
                                                       keys-p)))
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
                ,@(if custom-test-supplied-p
                      `(:custom-test ',custom-test)
                      `(:test ',test)))
               (values-list ,return-values))))))))

;;;; DEFRECGENERIC section

;; Observations:
;; We can't easily test the functional interface of a single method, since there's
;; not really any clean way to call it. The various :arounds, :befores, :afters
;; might be very relevant to what the output of the generic call looks like (e.g.
;; by setting up context appropriately). Hence, we can't do DEFRECMETHOD, but I
;; think we _can_ do DEFRECGENERIC!

;; Ideas:
;; 1) In case of &key &allow-other-keys without a &rest, we'll need to insert
;; an automatic &rest I think. Verified quickly in the REPL that this doesn't
;; lead to trouble, though, so's all good :)
;; 2) Supply a weirdo method combination to ensure we can wrap stuff around
;; the whole gamut. Obviously, if method combination stuff gets supplied by
;; the user (in the DEFRECGENERIC!!) form, then we error and warn that it's
;; not supported. Supplying an alternate method combination is obviously a
;; little messy, but while not perfect it at least meets the goal of easily
;; slotting into the common cases. (Surely 95%+, at least, of generics use
;; the standard method combination?)

;; Attack:
;; CLHS (http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm)
;; contains, as an example, the following:

;; ;The default method-combination technique
;;  (define-method-combination standard ()
;;          ((around (:around))
;;           (before (:before))
;;           (primary () :required t)
;;           (after (:after)))
;;    (flet ((call-methods (methods)
;;             (mapcar #'(lambda (method)
;;                         `(call-method ,method))
;;                     methods)))
;;      (let ((form (if (or before after (rest primary))
;;                      `(multiple-value-prog1
;;                         (progn ,@(call-methods before)
;;                                (call-method ,(first primary)
;;                                             ,(rest primary)))
;;                         ,@(call-methods (reverse after)))
;;                      `(call-method ,(first primary)))))
;;        (if around
;;            `(call-method ,(first around)
;;                          (,@(rest around)
;;                           (make-method ,form)))
;;            form))))

;; My idea is to introduce a new method-combination, SUPERSTANDARD, which has
;; the STANDARD behaviour except for one modification: Allow the specification of
;; SUPERAROUND which wraps around all the arounds again (and around all the
;; before/after if there's no arounds). Only permit a single SUPERAROUND method,
;; error out and warn if there's multiple. Users should never specify a SUPERAROUND
;; themselves, it's simply there as part of the machinery to get DEFRECGENERIC
;; to work.

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
