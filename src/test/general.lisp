(in-package :autoCLaracterization)

(5am:def-suite :autoCLaracterization)

(defun load-defrec (defun-form &key test custom-test (strategy :entry-only))
  (destructuring-bind (def name &rest rest) defun-form
    (assert (member def '(defun defgeneric defmethod) :test #'eq))
    (assert (symbolp name))
    (assert (member strategy '(:all :outer-only :entry-only)))
    (eval
     `(,(ecase def
          (defun 'defrecfun)
          (defgeneric 'defrecgeneric)
          (defmethod 'defrecmethod))
       (,name ,@(when test `(:test ,test))
              ,@(when custom-test `(:custom-test ,custom-test))
              :strategy ,strategy)
       ,@rest))
    (compile name)))

(defmacro with-defrec-preamble ((var) form &body body)
  `(let ((*characterization-tests* (make-hash-table))
         (*recorder-lock* (bt:make-lock "recorder-lock-fib-test"))
         (*functions-to-instrument* (make-hash-table :test #'eq))
         (*form-buffer* (make-instance 'fixed-size-buffer :size 2))
         (,var ',form))
     #+sbcl(declaim (sb-ext:muffle-conditions style-warning))
     ,@body))

(defun remove-fn (name)
  (when (fboundp name)
    (let ((fn (symbol-function name)))
      (when (typep fn 'generic-function)
        (dolist (method (c2mop:generic-function-methods fn))
          (remove-method fn method))))
    (fmakunbound name)))

(defmacro with-redefinition-mimicker (name strings-to-check &body body)
  "Just to insistently redefine along similar lines as offered interactively.

NAME is the name of the function under consideration.

STRINGS-TO-CHECK is a list of strings (or a single string) which, if a subsequence of the error message, triggers retrying with undefining the current
fn."
  (let ((label-name (gensym "WITH-REDEFINITION-LABEL-BODY"))
        (retry-tag (gensym "WITH-REDEFINITION-RETRY-TAG"))
        (counter (gensym "WITH-REDEFINITION-COUNTER")))
    `(let ((,counter 1))
       (labels ((,label-name ()
                  ,@body))
         (tagbody ,retry-tag
            (handler-bind ((error
                             (lambda (e)
                               (let ((report (format nil "~A" e)))
                                 (when (and (plusp ,counter)
                                            (some (lambda (sub)
                                                    (search sub report))
                                                  ',(listify strings-to-check)))
                                   (decf ,counter)
                                   (remove-fn ',name)
                                   (go ,retry-tag))))))
              (,label-name)))))))
