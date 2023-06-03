(in-package :autoCLaracterization)

;;;; High-level forms

(defun generate-result-form (return-values)
  `(list ,@(mapcar #'serialize-object return-values)))

(defun generate-invocation-form (name &key
                                        required-params
                                        optional-params
                                        rest-param
                                        keyword-params)
  ;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node64.html
  ;; (required &optional optional &rest rest &key keys)
  ;; If we have a REST-PARAM, it will automatically capture KEYWORD-PARAMS :)
  `(,name ,@(mapcar #'serialize-object
                    (append required-params optional-params (if rest-param
                                                                rest-param
                                                                keyword-params)))))

;;;; Code below has largely been copied from https://github.com/arthev/spiclum

;;;; Instantiators
(defun instancify (instance-args &rest pairs)
  (assert (evenp (length pairs)))
  (prog1-let (instance (apply #'make-instance instance-args))
    (loop for (slot-name value) on pairs by #'cddr
          do (setf (slot-value instance slot-name) value))))

(defun hashify (hash-args &rest pairs)
  (assert (evenp (length pairs)))
  (prog1-let (ht (apply #'make-hash-table hash-args))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key ht) value))))

(defun arrayify (array-args &rest elts)
  (prog1-let (array (apply #'make-array array-args))
    (loop for i from 0
          for elt in elts
          do (setf (row-major-aref array i) elt))))

;;;; Object serializers

(defgeneric serialize-object (obj)
  (:documentation "Generic to serialize arbitrary objects.

Recursive in many cases, since composite values like lists, structs, can only be serializable if their elementary values are serializable.

Can't serialize everything, given that some data types (e.g. anonymous functions) lack any introspectability."))

(defmethod serialize-object ((number number))
  number)

(defmethod serialize-object ((symbol symbol))
  (if (keywordp symbol)
      symbol
      `',symbol))

(defmethod serialize-object ((character character))
  character)

(defmethod serialize-object ((string string))
  (if (simple-string-p string)
      string
      (call-next-method)))

(defmethod serialize-object ((state random-state))
  ;; http://www.lispworks.com/documentation/lw50/CLHS/Body/t_rnd_st.htm
  ;; requires random-states to have a readable printable representation.
  state)

(defmethod serialize-object ((list list))
  ;; See the following link for discussion on different list types that need special handling:
  ;; https://stackoverflow.com/questions/60247877/check-for-proper-list-in-common-lisp
  (cond ((null list) nil)
        ((null (cdr (last list))) `(list ,@(mapcar #'serialize-object list)))
        (t `(cons ,(serialize-object (car list))
                  ,(serialize-object (cdr list))))))

(defmethod serialize-object ((array array))
  (let ((fill-pointer (ignore-errors (fill-pointer array))))
    `(arrayify
      '(,(if fill-pointer
             fill-pointer
             (array-dimensions array))
        :element-type ,(array-element-type array)
        :adjustable ,(adjustable-array-p array)
        :fill-pointer ,fill-pointer)
      ,@(loop for i from 0 below (if fill-pointer
                                     fill-pointer
                                     (reduce #'* (array-dimensions array)))
              collect (serialize-object (row-major-aref array i))))))

(defmethod serialize-object ((ht hash-table))
  `(hashify
    (list
     :test ,`(function ,(hash-table-test ht))
     :size ,(hash-table-size ht)
     :rehash-size ,(hash-table-rehash-size ht)
     :rehash-threshold ,(hash-table-rehash-threshold ht))
    ,@(loop for key being the hash-keys of ht
            collect (serialize-object key)
            collect (serialize-object (gethash key ht)))))

(defmethod serialize-object ((class standard-class))
  (assert (class-name class))
  `(find-class ',(class-name class)))

(defmethod serialize-object ((instance standard-object))
  (let (initargs value-maps)
    (do-bound-slots (slotd instance :name slot-name :value slot-value)
      (let ((serialized-slot-value (serialize-object slot-value)))
        (prependf value-maps `(',slot-name ,serialized-slot-value))
        (lwhen (initarg (car (c2mop:slot-definition-initargs slotd)))
          (prependf initargs `(,initarg ,serialized-slot-value)))))
    `(instancify
      (list ',(class-name (class-of instance)) ,@initargs)
      ,@value-maps)))
