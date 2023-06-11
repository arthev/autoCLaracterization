(in-package :autoCLaracterization)

;;;; Registering to instrument

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *functions-to-instrument* (make-hash-table :test #'eq)
    "Maps NAME -> NAME-AND-OPTIONS per DEFRECFUN/DEFRECGENERIC")
  )

(defmacro register-recorder (name &key
                                    (test '#'eql test-supplied-p)
                                    (custom-test nil custom-test-supplied-p)
                                    (strategy *default-recorder-strategy*))
  "Instruct the autoCLaracterization system to instrument function NAME,
whether it be a generic function or not (and whether a defgeneric form exists
or not).

Does NOT preserve lexical environment, so e.g. CUSTOM-TEST can't closure."
  (when (and test-supplied-p custom-test-supplied-p)
    (error "REGISTER-RECORDER for ~A supplies both TEST and CUSTOM-TEST."
           name))
  `(setf (gethash ',name *functions-to-instrument*)
         (list ',name
               ,@(when test-supplied-p `(:test ',test))
               ,@(when custom-test-supplied-p `(:custom-test ',custom-test))
               :strategy ,strategy)))

;;;; Interceptions and transformations

(defvar *old-macroexpand-hook* *macroexpand-hook*)

(defparameter *form-buffer* (make-instance 'fixed-size-buffer :size 10))

(defun recently-seen-form-p (form)
  (member form (buffer-contents *form-buffer*) :test #'equal))

(defun register-seen-form (form)
  (buffer-push *form-buffer* (copy-tree form)))

(defun autoCLaracterization-macroexpand-hook (expander form env)
  "Intercepts and possibly replaces defuns/defgenerics/defmethods with defrecfun/defrecgenerics.

Does this by analysing FORM, deciding whether it's been set as a function to instrument, and then doing so.

Tries to play nice in presence of other hooks by storing old hook and calling out to it with possibly replaced EXPANDER and FORM."
  (multiple-value-bind (dexpander dform denv) ; d for derived
      (if (recently-seen-form-p form)
          (values expander form env)
          (progn
            (register-seen-form form)
            (case (car form)
              (defun
                  (defun-hook-args expander form env))
              (defgeneric
                  (defgeneric-hook-args expander form env))
              (defmethod
                  (defmethod-hook-side-effects expander form env)
                  (values expander form env))
              (otherwise
               (values expander form env)))))
    (let* ((result-values
             (multiple-value-list
              (funcall *old-macroexpand-hook* dexpander dform denv)))
           (result-form (car result-values)))
      (unless (equal form dform)
        (register-seen-form dform)
        (register-seen-form result-form))
      (values-list result-values))))

(defun transform-for-recording (expander form env new-wrapper name-and-options)
  (destructuring-bind (def name lambda-list &body body) form
    (declare (ignore def name))
    (values
     (macro-function new-wrapper env)
     `(,new-wrapper ,name-and-options ,lambda-list ,@body)
     env)))

(defun maybe-transform-for-recording (expander form env new-wrapper)
  (destructuring-bind (def name lambda-list &body body) form
    (declare (ignore lambda-list body))
    (assert (member def '(defun defgeneric) :test #'eq))
    (assert (symbolp name))
    (lif (name-and-options (gethash name *functions-to-instrument*))
         (transform-for-recording expander form env new-wrapper name-and-options)
         (values expander form env))))

(defun defun-hook-args (expander form env)
  (maybe-transform-for-recording expander form env 'defrecfun))

(defun defgeneric-hook-args (expander form env)
  (maybe-transform-for-recording expander form env 'defrecgeneric))

(defun defmethod-hook-side-effects (expander form env)
  (declare (ignore expander form env))
  :todo)
