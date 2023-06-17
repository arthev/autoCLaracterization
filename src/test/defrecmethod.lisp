(in-package :autoCLaracterization)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(5am:def-suite :defrecgeneric :in :autoCLaracterization)

(5am:in-suite :defrecgeneric)

(5am:test :boring-defrecmethod ; a simple happy case
  (with-defrec-preamble (fibdef)
                        (defmethod fib (n)
                          (let ((s5 (sqrt 5)))
                            (nth-value
                             0
                             (round
                              (* (/ 1 s5)
                                 (- (expt (/ (+ 1 s5) 2) n)
                                    (expt (/ (- 1 s5) 2) n)))))))
    (remove-fn 'fib)
    (load-defrec fibdef :test '#'=)
    (5am:is (= 1 (length (methods-by-qualifiers 'fib '(:superaround)))))
    (5am:is (= 2 (length (c2mop:generic-function-methods (symbol-function 'fib)))))
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

(5am:test :congruent-generic-method-lambda-lists-p-gauntlet
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '() '() '()))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(a b) '(a b) '()))
  (5am:is-false (congruent-generic-method-lambda-lists-p
                 '(a b) '(a ) '()))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(a &optional b) '(c &optional d) '()))
  (5am:is-false (congruent-generic-method-lambda-lists-p
                 '(a &optional b) '(c &optional d e) '()))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(a &rest r) '(c &rest k) '()))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(a &rest r) '(c &key i j k &allow-other-keys) '()))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(a &rest r) '(c &key i j k) '(g &key m &allow-other-keys)))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(d &key a b c) '(e &key &allow-other-keys) '()))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(d &key a b c) '(f &key a b c) '()))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(d &key a b c) '(g &rest r) '()))
  (5am:is-true (congruent-generic-method-lambda-lists-p
                '(d &key a b c) '(g &rest r &key a b c) '()))
  (5am:is-false (congruent-generic-method-lambda-lists-p
                 '(d &key a b c) '(g &rest r &key f) '()))
  )
