(in-package :autoCLaracterization)

;;;; Functional programming

(defun lfix (fn &rest args)
  "Do a partial function application,
   by fixing the leftmost args as ARGS."
  (lambda (&rest more-args)
    (apply fn (append args more-args))))

;;;; Normalization

(defun listify (x) (if (listp x) x (list x)))

(defun keywordify (arg)
  (intern (ctypecase arg
            (string arg)
            (symbol (symbol-name arg)))
          :keyword))

;;;; Iterators

(defmacro do-bound-slots ((slot-var instance
                           &key
                             (slots nil slots-supplied-p)
                             (value (gensym) value-supplied-p)
                             (name (gensym)))
                          &body body)
  (let ((slots (if slots-supplied-p
                   slots
                   `(c2mop:class-slots (class-of ,instance)))))
    `(dolist (,slot-var ,slots)
       (let ((,name (c2mop:slot-definition-name ,slot-var)))
         (when (slot-boundp ,instance ,name)
           (let ((,value ,(if value-supplied-p
                              `(slot-value ,instance ,name)
                              nil)))
             ,@body))))))

;;;; Syntax

(defmacro prog1-let ((var form) &body body)
  `(let ((,var ,form))
     (prog1 ,var ,@body)))

(defmacro lwhen ((var pred) &body body)
  "let-when. Bind VAR to val of PRED within body."
  `(let ((,var ,pred))
     (when ,var
       ,@body)))

(defmacro lif ((var pred) consequent alternative)
  "let-if. Bind VAR to val of PRED within consequent/alternative."
  `(let ((,var ,pred))
     (if ,var
         ,consequent
         ,alternative)))

(define-modify-macro prependf (head)
  (lambda (tail head)
    (append head tail))
  "See https://stackoverflow.com/questions/17908564/what-is-to-append-as-push-is-to-cons-in-lisp")

;;;; Macrology

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars
               collect (cond ((symbolp var)
                              `(,var (gensym ,(symbol-name var))))
                             ((and (listp var)
                                   (= 2 (length var))
                                   (symbolp (car var))
                                   (stringp (cadr var)))
                              `(,(car var) (gensym ,(cadr var))))
                             (t
                              (error "Unexpected VAR format: ~S~% expected either SYMBOL or (SYMBOL STRING)." var))))
     ,@body))

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

(defun congruent-generic-method-lambda-lists-p
    (generic-lambda-list method-lambda-list all-method-lambda-lists)
  "Checks if GENERIC-LAMBDA-LIST and METHOD-LAMBDA-LIST are congruent,
 as per cs.cmu.edu/Groups/AI/html/cltl/clm/node281.html.

GENERIC-LAMBDA-LIST is the lambda-list for the generic function.
METHOD-LAMBDA-LIST is the lambda-list for the method under consideration.
ALL-METHOD-LAMBDA-LIST is the set of lambda-lists for all methods belonging to the generic function, because of the &allow-other-keys bullet (number 5 in the list)."
  (multiple-value-bind (required-params-1
                        optional-params-1
                        rest-param-1
                        keyword-params-1
                        allow-other-keys-p-1
                        aux-params-1
                        keys-p-1)
      (alexandria:parse-ordinary-lambda-list generic-lambda-list)
    (multiple-value-bind (required-params-2
                          optional-params-2
                          rest-param-2
                          keyword-params-2
                          allow-other-keys-p-2
                          aux-params-2
                          keys-p-2)
        (alexandria:parse-ordinary-lambda-list method-lambda-list)
      (and
       ;; Same number of requireds
       (= (length required-params-1)
          (length required-params-2))
       ;; Same number of optionals
       (= (length optional-params-1)
          (length optional-params-2))
       ;; If either mentions &rest or &key, then both must
       (if (or rest-param-1 rest-param-2 keys-p-1 keys-p-2)
           (and (or rest-param-1 keys-p-1)
                (or rest-param-2 keys-p-2))
           t)
       ;; If the generic mentions &key, then methods must accept them
       (if keys-p-1
           (or allow-other-keys-p-1
               allow-other-keys-p-2
               (and rest-param-2 (not keys-p-2))
               ;; accepts all keyword names?
               (every (lambda (key-param-1)
                        ;; normalized, hence key params on form
                        ;; ((keyword-name var) something suppliedp)
                        (member (caar key-param-1) keyword-params-2 :key #'caar))
                      keyword-params-1)
               (some (lambda (method-lambda-list)
                       (member '&allow-other-keys method-lambda-list))
                     all-method-lambda-lists))
           t)))))

;;;; Ring buffer section

(defclass fixed-size-buffer ()
  ((size :initarg :size
         :accessor size)
   (head :accessor head
         :initform nil)
   (tail :accessor tail
         :initform nil)
   (buffer-count :accessor buffer-count
                 :initform 0)))

(defmethod buffer-push ((buffer fixed-size-buffer) obj)
  (cond ((null (head buffer))
         (setf (head buffer) (list obj)
               (tail buffer) (head buffer)))
        ((< (buffer-count buffer) (size buffer))
         (setf (cdr (tail buffer)) (list obj)
               (tail buffer) (cdr (tail buffer))
               (buffer-count buffer) (1+ (buffer-count buffer))))
        (t
         (setf (cdr (tail buffer)) (list obj)
               (tail buffer) (cdr (tail buffer))
               (head buffer) (cdr (head buffer))))))

(defmethod buffer-contents ((buffer fixed-size-buffer))
  (head buffer))

;;;; MOPy stuff

(defgeneric methods-by-qualifiers (generic-fn qualifiers)
  (:documentation "Returns the set of methods for GENERIC-FN matching QUALIFIERS.

GENERIC-FN designates the generic fn (either by being the generic-fn or by naming it.

QUALIFIERS is the list of qualifiers (possibly nil)."))

(defmethod methods-by-qualifiers ((name symbol) qualifiers)
  (methods-by-qualifiers (symbol-function name) qualifiers))

(defmethod methods-by-qualifiers ((fn generic-function) qualifiers)
  (let ((sorted-qualifiers (sort (copy-list qualifiers) #'string<)))
    (remove-if
     (lambda (method)
       (not
        (equal sorted-qualifiers
               (sort (copy-list (c2mop::method-qualifiers method)) #'string<))))
     (c2mop:generic-function-methods fn))))
