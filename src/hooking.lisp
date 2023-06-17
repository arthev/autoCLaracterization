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

(defparameter *form-buffer*
  ;; We don't want to keep around more forms than "the current" recorder
  ;; macroexpansion requires, ideally. Well, DEFRECFUN requires 1 form (the defun),
  ;; while DEFRECGENERIC requires two: (progn (defgeneric ...) (defmethod ...))
  ;; Might be DEFMETHOD will require more than one two. But in any case, we can
  ;; simply set the size to the max of these and then insert multiples of the
  ;; fewer forms. Potentially find a least common multiple as size, if necessary.
  ;; Since we only register new forms when we encounter _new_ defuns etc., this
  ;; ought to work out nicely.
  (make-instance 'fixed-size-buffer :size 2))

(defun recently-seen-form-p (form)
  (member form (buffer-contents *form-buffer*) :test #'equal))

(defgeneric register-seen-form (type form)
  (:documentation "Cross-reference with DEFRECFUN, DEFRECGENERIC, etc."))

(defmethod register-seen-form ((type (eql 'defun)) form)
  (unless (eq 'defun (car form))
    (error "REGISTER-SEEN-FORM for 'DEFUN invoked on FORM not conforming to output of DEFRECFUN: ~S" form))
  (dotimes (i 2)
    (buffer-push *form-buffer* form)))

(defmethod register-seen-form ((type (eql 'defgeneric)) form)
  (destructuring-bind (sprog1 defgeneric-form defmethod-form) form
    (unless (and (eq 'prog1 sprog1)
                 (eq 'defgeneric (car defgeneric-form))
                 (eq 'defmethod (car defmethod-form)))
      (error "REGISTER-SEEN-FORM for 'DEFGENERIC invoked on FORM not conforming to output of DEFRECGENERIC: ~S" form))
    (buffer-push *form-buffer* defgeneric-form)
    (buffer-push *form-buffer* defmethod-form)))

(defmethod register-seen-form ((type (eql 'defmethod)) form)
  (destructuring-bind (sprogn defrecgeneric-form defmethod-form) form
    (unless (and (eq 'progn sprogn)
                 (eq 'defrecgeneric (car defrecgeneric-form))
                 (eq 'defmethod (car defmethod-form)))
      (error "REGISTER-SEEN-FORM for 'DEFMETHOD invoked on FORM not conforming to output of DEFRECMETHOD: ~S" form)))
  ;; Heavy work's off-loaded to the defrecgeneric subform, so don't push anything
  ;; to the *form-buffer*.
  :ok)

(defun autoCLaracterization-macroexpand-hook (expander form env)
  "Intercepts and possibly replaces defuns/defgenerics/defmethods with defrecfun/defrecgenerics.

Does this by analysing FORM, deciding whether it's been set as a function to instrument, and then doing so.

Tries to play nice in presence of other hooks by storing old hook and calling out to it with possibly replaced EXPANDER and FORM."
  (multiple-value-bind (dexpander dform denv transformed-for) ; d for derived
      (if (recently-seen-form-p form)
          (values expander form env nil)
          (case (car form)
            (defun
                (defun-hook-args expander form env))
            (defgeneric
                (defgeneric-hook-args expander form env))
            (defmethod
                (defmethod-hook-args expander form env))
            (defrecgeneric
                ;; This simply sets up recording of the two generated subforms,
                ;; so that the defmethod won't trigger infinite iteration because
                ;; of the 'no superaround exists yet' check.
                (values expander form env 'defgeneric))
            (otherwise
             (values expander form env nil))))
    (let ((result-values
            (multiple-value-list
             (funcall *old-macroexpand-hook* dexpander dform denv))))
      (when transformed-for
        ;; If we've done a transformation, register having seen the result-form
        ;; of _expanding the transformed form_. This means we should then not
        ;; subsequently end up re-transforming the result form and so forth.
        (register-seen-form transformed-for (car result-values)))
      (values-list result-values))))

(defun transform-for-recording (expander form env new-wrapper name-and-options)
  (declare (ignore expander))
  (destructuring-bind (def name &rest rest) form
    (declare (ignore name))
    (values
     (macro-function new-wrapper env)
     `(,new-wrapper ,name-and-options ,@rest)
     env
     def)))

(defun maybe-transform-for-recording (expander form env new-wrapper)
  (destructuring-bind (def name &rest rest) form
    (declare (ignore rest))
    (assert (member def '(defun defgeneric defmethod) :test #'eq))
    (assert (symbolp name))
    (lif (name-and-options (gethash name *functions-to-instrument*))
         (transform-for-recording expander form env new-wrapper name-and-options)
         (values expander form env nil))))

(defun defun-hook-args (expander form env)
  (maybe-transform-for-recording expander form env 'defrecfun))

(defun defgeneric-hook-args (expander form env)
  (maybe-transform-for-recording expander form env 'defrecgeneric))

(defun defmethod-hook-args (expander form env)
  (let* ((name (car (listify (cadr form))))
         (lambda-list (c2mop:extract-lambda-list (find-if #'listp (cddr form))))
         (gen-fn (when (fboundp name) (symbol-function name))))
    (if (or (not (fboundp name))
            (not (methods-by-qualifiers name '(:superaround)))
            (not (congruent-generic-method-lambda-lists-p
                  (c2mop:generic-function-lambda-list gen-fn)
                  lambda-list
                  (mapcar #'c2mop:method-lambda-list
                          (c2mop:generic-function-methods gen-fn)))))
        (maybe-transform-for-recording expander form env 'defrecmethod)
        (values expander form env nil))))
