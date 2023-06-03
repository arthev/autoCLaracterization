(in-package :autoCLaracterization)

(defun listify (x) (if (listp x) x (list x)))

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

(defmacro prog1-let ((var form) &body body)
  `(let ((,var ,form))
     (prog1 ,var ,@body)))

(define-modify-macro prependf (head)
  (lambda (tail head)
    (append head tail))
  "See https://stackoverflow.com/questions/17908564/what-is-to-append-as-push-is-to-cons-in-lisp")

(defmacro lwhen ((var pred) &body body)
  "let-when. Bind VAR to val of PRED within body."
  `(let ((,var ,pred))
     (when ,var
       ,@body)))

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

(defun keywordify (arg)
  (intern (ctypecase arg
            (string arg)
            (symbol (symbol-name arg)))
          :keyword))
