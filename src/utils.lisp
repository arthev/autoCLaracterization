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
