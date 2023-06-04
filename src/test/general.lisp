(in-package :autoCLaracterization)

(5am:def-suite :autoCLaracterization)

(defun load-defrecfun (defun-form &key test custom-test)
  (destructuring-bind (def name lambda-list &body body) defun-form
    (assert (eq 'defun def))
    (assert (symbolp name))
    (compile
     (eval
      `(defrecfun (,name ,@(when test `(:test ,test))
                          ,@(when custom-test `(:custom-test ,custom-test)))
           ,lambda-list
         ,@body)))))

(defmacro with-defrecfun-preamble ((var) form &body body)
  `(let ((*characterization-tests* (make-hash-table))
         (*recorder-lock* (bt:make-lock "recorder-lock-fib-test"))
         (,var ',form))
     ,@body))
