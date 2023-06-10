(in-package :autoCLaracterization)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(5am:def-suite :defrecgeneric :in :autoCLaracterization)

(5am:in-suite :defrecgeneric)

(defvar *tracker* nil)

(defvar *superstandard-test-forms*
  '((defgeneric superstandard-test (x)
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
      (push `(:superaround t) *tracker*))))

(5am:test :superstandard-method-combination
  ;; Observe that, from most specific to least specific:
  ;; integer -> rational -> real -> number -> t
  (let ((*tracker* nil))
    (remove-fn 'superstandard-test)
    (dolist (form *superstandard-test-forms*)
      (eval form))
    (superstandard-test 5)
    (5am:is (equal
             '((:superaround t)
               (:around :integer) (:around :rational) (:around :number)
               (:after :integer) (:after :rational) (:after :number)
               (:primary :integer) (:primary :rational) (:primary :number)
               (:before :number) (:before :rational) (:before :integer))
             *tracker*))
    (eval '(defmethod superstandard-test :superaround ((x number))
             (call-next-method)
             (push `(:superaround :number) *tracker*)))
    (handler-case
        (progn
          (superstandard-test 5)
          (5am:fail "Failed to error out SUPERSTANDARD-TEST call despite multiple :superaround methods present."))
      (multiple-superarounds-error ()
        (5am:pass "Correctly errored out in presence of multiple :superarounds")))))

(5am:test :boring-defrecgeneric ; a simple happy case
  (with-defrec-preamble (fibdefgeneric)
                        (defgeneric fib (n))
    (remove-fn 'fib)
    (load-defrec fibdefgeneric :test '#'=)
    (eval '(defmethod fib (n)
            (let ((s5 (sqrt 5)))
              (nth-value
               0
               (round
                (* (/ 1 s5)
                   (- (expt (/ (+ 1 s5) 2) n)
                      (expt (/ (- 1 s5) 2) n))))))))
    (compile 'fib)
    (5am:is (=   1 (fib 1)))
    (5am:is (=   8 (fib 6)))
    (5am:is (= 610 (fib 15)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 3 (length (gethash 'fib *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'= (list 1)   (multiple-value-list (fib  1))))
              (5am:is (every #'= (list 8)   (multiple-value-list (fib  6))))
              (5am:is (every #'= (list 610) (multiple-value-list (fib 15)))))
            (reverse (gethash 'fib *characterization-tests*))))))

(5am:test :rest-included-generic ; test proper capture when &rest included
  (with-defrec-preamble (some-gen)
                        (defgeneric some-fn (a b
                                             &optional c
                                             &rest r
                                             &key d e &allow-other-keys))
    (remove-fn 'some-fn)
    (load-defrec some-gen :test '#'equal)
    (eval '(defmethod some-fn (a b &optional c &key f &allow-other-keys)
            (list :a a :b b :c c :f f)))
    (compile 'some-fn)
    (5am:is (equal '(:a summer :b winter :c autumn :f spring)
                   (some-fn 'summer
                            'winter
                            'autumn
                            :e 'potato
                            :f 'spring
                            :g 'nocturne)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 1 (length (gethash 'some-fn *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'equal (list (list :a 'summer
                                                 :b 'winter
                                                 :c 'autumn
                                                 :f 'spring))
                                     (multiple-value-list
                                      (some-fn
                                       'summer
                                       'winter
                                       'autumn
                                       :e 'potato
                                       :f 'spring
                                       :g 'nocturne)))))
            (reverse (gethash 'some-fn *characterization-tests*))))))

(5am:test :rest-unincluded-generic ; test proper capture when &rest not included
  (with-defrec-preamble (some-gen)
                        (defgeneric some-fn (a b
                                             &optional c
                                             &key d e &allow-other-keys))
    (remove-fn 'some-fn)
    (load-defrec some-gen :test '#'equal)
    (eval '(defmethod some-fn (a b &optional c &key f &allow-other-keys)
            (list :a a :b b :c c :f f)))
    (compile 'some-fn)
    (5am:is (equal '(:a summer :b winter :c autumn :f spring)
                   (some-fn 'summer
                            'winter
                            'autumn
                            :e 'potato
                            :f 'spring
                            :g 'nocturne)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 1 (length (gethash 'some-fn *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'equal (list (list :a 'summer
                                                 :b 'winter
                                                 :c 'autumn
                                                 :f 'spring))
                                     (multiple-value-list
                                      (some-fn
                                       'summer
                                       'winter
                                       'autumn
                                       :e 'potato
                                       :f 'spring
                                       :g 'nocturne)))))
            (reverse (gethash 'some-fn *characterization-tests*))))))

(5am:test :generic-optional ; test proper capture when reqs/optionals only
  (with-defrec-preamble (some-gen)
                        (defgeneric some-fn (a b
                                             &optional c))
    (remove-fn 'some-fn)
    (load-defrec some-gen :test '#'equal)
    (eval '(defmethod some-fn (a b &optional c)
            (list :a a :b b :c c)))
    (compile 'some-fn)
    (5am:is (equal '(:a summer :b winter :c autumn)
                   (some-fn 'summer
                            'winter
                            'autumn)))
    (5am:is (equal '(:a summer :b winter :c nil)
                   (some-fn 'summer
                            'winter)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 2 (length (gethash 'some-fn *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'equal (list (list :a 'summer
                                                 :b 'winter
                                                 :c 'autumn))
                                     (multiple-value-list
                                      (some-fn
                                       'summer
                                       'winter
                                       'autumn))))
              (5am:is (every #'equal (list (list :a 'summer
                                                 :b 'winter
                                                 :c nil))
                                     (multiple-value-list
                                      (some-fn
                                       'summer
                                       'winter)))))
            (reverse (gethash 'some-fn *characterization-tests*))))))
