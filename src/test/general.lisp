(in-package :autoCLaracterization)

(5am:def-suite :autoCLaracterization)

(defun load-defrec (defun-form &key test custom-test)
  (destructuring-bind (def name lambda-list &body rest) defun-form
    (assert (member def '(defun defgeneric) :test #'eq))
    (assert (symbolp name))
    (eval
     `(,(if (eq def 'defun)
            'defrecfun
            'defrecgeneric)
       (,name ,@(when test `(:test ,test))
              ,@(when custom-test `(:custom-test ,custom-test)))
       ,lambda-list
       ,@rest))
    (compile name)))

(defmacro with-defrec-preamble ((var) form &body body)
  `(let ((*characterization-tests* (make-hash-table))
         (*recorder-lock* (bt:make-lock "recorder-lock-fib-test"))
         (,var ',form))
     ,@body))

(defun remove-fn (name)
  (when (fboundp name)
    (let ((fn (symbol-function name)))
      (when (typep fn 'generic-function)
        (dolist (method (c2mop:generic-function-methods fn))
          (remove-method fn method))))
    (fmakunbound name)))
