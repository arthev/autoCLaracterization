(in-package :autoCLaracterization)

(5am:def-suite :defrecgeneric :in :autoCLaracterization)

(5am:in-suite :defrecgeneric)

(define-method-combination superstandard ()
        ((around (:around))
         (superaround (:superaround))
         (before (:before))
         (primary () :required t)
         (after (:after) :order :most-specific-last))
  "SUPERSTANDARD method combination to wrap a single :superaround around an
otherwise standard method combination using generic. Hence instrumentive behaviour
can be added without modifying any existing DEFMETHOD forms. Balks if multiple
:superaround exist for a given generic function."
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods after))
                    `(call-method ,(first primary)))))
      (cond (superaround
             (if (< 1 (length superaround))
                 (invalid-method-error
                  (car superaround)
                  "Only 1 :superaround permitted per generic, but found: ~S"
                  superaround)
                 `(call-method ,(first superaround)
                               (,@around
                                (make-method ,form)))))
            (around
             `(call-method ,(first around)
                           (,@(rest around)
                            (make-method ,form))))
            (t
             form)))))

(defvar *tracker* nil)

(defgeneric superstandard-test (x)
  (:method-combination superstandard))
(defmethod superstandard-test ((x number))
  (push `(:primary :number) *tracker*))
(defmethod superstandard-test ((x rational))
  (call-next-method)
  (push `(:primary :rational) *tracker*))
(defmethod superstandard-test ((x integer))
  (call-next-method)
  (push `(:primary :integer) *tracker*))
(defmethod superstandard-test :before ((x number))
  (push `(:before :number) *tracker*))
(defmethod superstandard-test :before ((x rational))
  (push `(:before :rational) *tracker*))
(defmethod superstandard-test :before ((x integer))
  (push `(:before :integer) *tracker*))
(defmethod superstandard-test :after ((x number))
  (push `(:after :number) *tracker*))
(defmethod superstandard-test :after ((x rational))
  (push `(:after :rational) *tracker*))
(defmethod superstandard-test :after ((x integer))
  (push `(:after :integer) *tracker*))
(defmethod superstandard-test :around ((x number))
  (call-next-method)
  (push `(:around :number) *tracker*))
(defmethod superstandard-test :around ((x rational))
  (call-next-method)
  (push `(:around :rational) *tracker*))
(defmethod superstandard-test :around ((x integer))
  (call-next-method)
  (push `(:around :integer) *tracker*))
(defmethod superstandard-test :superaround (x)
  (call-next-method)
  (push `(:superaround t) *tracker*))

(5am:test :superstandard-method-combination
  ;; Observe that, from most specific to least specific:
  ;; integer -> rational -> real -> number -> t
  (let ((*tracker* nil))
    (superstandard-test 5)
    (5am:is (equal
             '((:superaround t)
               (:around :integer) (:around :rational) (:around :number)
               (:after :integer) (:after :rational) (:after :number)
               (:primary :integer) (:primary :rational) (:primary :number)
               (:before :number) (:before :rational) (:before :integer))
              *tracker*))))
