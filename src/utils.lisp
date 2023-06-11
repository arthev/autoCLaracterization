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
