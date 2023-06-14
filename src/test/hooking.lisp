(in-package :autoCLaracterization)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(5am:def-suite :hooking :in :autoCLaracterization)

(5am:in-suite :hooking)

(5am:test :basic-defun-instrumentation
  (remove-fn 'fib)
  (with-defrec-preamble (fibdefun) (defun fib (n)
                                     (if (<= n 2)
                                         1
                                         (+ (fib (- n 1))
                                            (fib (- n 2)))))
    (let ((*macroexpand-hook* 'autoCLaracterization-macroexpand-hook))
      (5am:is (= 0 (hash-table-count *functions-to-instrument*)))
      (eval '(register-recorder fib :test #'=))
      (5am:is (= 1 (hash-table-count *functions-to-instrument*)))
      (compile (eval fibdefun))
      (5am:is-true (fboundp 'fib))
      (5am:is (= 0 (hash-table-count *characterization-tests*)))
      (5am:is (= 610 (fib 15)))
      (5am:is (= 1 (hash-table-count *characterization-tests*)))
      (5am:is (= 1 (length (gethash 'fib *characterization-tests*))))
      (5am:is
       (equal '((5am:is (every #'= (list 610) (multiple-value-list (fib 15)))))
              (reverse (gethash 'fib *characterization-tests*)))))))

(5am:test :basic-defun-redefinitions
  (remove-fn 'some-fn)
  (with-defrec-preamble (somefun) (defun some-fn ()
                                    'a)
    (let ((*macroexpand-hook* 'autoCLaracterization-macroexpand-hook))
      (5am:is (= 0 (hash-table-count *functions-to-instrument*)))
      (eval '(register-recorder some-fn))
      (5am:is (= 1 (hash-table-count *functions-to-instrument*)))
      (compile (eval somefun))
      (5am:is-true (fboundp 'some-fn))
      (5am:is (= 0 (hash-table-count *characterization-tests*)))
      (5am:is (eql 'a (some-fn)))
      (5am:is (= 1 (hash-table-count *characterization-tests*)))
      (5am:is (= 1 (length (gethash 'some-fn *characterization-tests*))))
      (5am:is
       (equal '((5am:is (every #'eql (list 'a) (multiple-value-list (some-fn)))))
              (reverse (gethash 'some-fn *characterization-tests*))))
      (compile (eval '(defun some-fn (n) (list n 'b))))
      (5am:is-true (fboundp 'some-fn))
      (5am:is (equal '(s b) (some-fn 's)))
      (5am:is (= 1 (hash-table-count *characterization-tests*)))
      (5am:is (= 2 (length (gethash 'some-fn *characterization-tests*))))
      (5am:is
       (equal '((5am:is (every #'eql (list 'a) (multiple-value-list (some-fn))))
                (5am:is (every #'eql (list (list 's 'b))
                                     (multiple-value-list (some-fn 's)))))
              (reverse (gethash 'some-fn *characterization-tests*)))))))

(5am:test :basic-defgeneric-instrumentation
  (remove-fn 'fib)
  (with-defrec-preamble (fibdef) (defgeneric fib (n))
    (let ((*macroexpand-hook* 'autoCLaracterization-macroexpand-hook))
      (5am:is (= 0 (hash-table-count *functions-to-instrument*)))
      (eval '(register-recorder fib :test #'=))
      (5am:is (= 1 (hash-table-count *functions-to-instrument*)))
      (eval fibdef)
      (compile 'fib)
      (5am:is-true (fboundp 'fib))
      (5am:is (= 0 (hash-table-count *characterization-tests*)))
      (5am:is (= 1 (length (c2mop:generic-function-methods #'fib))))
      (5am:is (equal '(:superaround)
                     (c2mop::method-qualifiers
                      (car
                       (c2mop:generic-function-methods #'fib)))))
      (eval '(defmethod fib (n)
              (let ((s5 (sqrt 5)))
                (nth-value
                 0
                 (round
                  (* (/ 1 s5)
                     (- (expt (/ (+ 1 s5) 2) n)
                        (expt (/ (- 1 s5) 2) n))))))))
      (5am:is (= 610 (fib 15)))
      (5am:is (= 1 (hash-table-count *characterization-tests*)))
      (5am:is (= 1 (length (gethash 'fib *characterization-tests*))))
      (5am:is
       (equal '((5am:is (every #'= (list 610) (multiple-value-list (fib 15)))))
              (reverse (gethash 'fib *characterization-tests*)))))))


(5am:test :basic-defgeneric-redefinitions
  (remove-fn 'some-fn)
  (with-defrec-preamble (somefun) (defgeneric some-fn (a))
    (let ((*macroexpand-hook* 'autoCLaracterization-macroexpand-hook))
      (5am:is (= 0 (hash-table-count *functions-to-instrument*)))
      (eval '(register-recorder some-fn))
      (5am:is (= 1 (hash-table-count *functions-to-instrument*)))
      (eval somefun)
      (5am:is-true (fboundp 'some-fn))
      (5am:is (= 0 (hash-table-count *characterization-tests*)))
      (5am:is (= 1 (length (c2mop:generic-function-methods #'some-fn))))
      (5am:is (equal '(:superaround)
                     (c2mop::method-qualifiers
                      (car
                       (c2mop:generic-function-methods #'some-fn)))))
      (eval '(defmethod some-fn (a)
              a))
      (5am:is (= 2 (length (c2mop:generic-function-methods #'some-fn))))
      (5am:is (eql 'c (some-fn 'c)))
      (5am:is (= 1 (hash-table-count *characterization-tests*)))
      (5am:is (= 1 (length (gethash 'some-fn *characterization-tests*))))
      (5am:is
       (equal '((5am:is (every #'eql (list 'c)
                                     (multiple-value-list (some-fn 'c)))))
              (reverse (gethash 'some-fn *characterization-tests*))))
      (remove-fn 'some-fn) ; since usually e.g. interactive restarts etc.
      (eval '(defgeneric some-fn (a b)))
      (5am:is-true (fboundp 'some-fn))
      (5am:is (= 1 (length (c2mop:generic-function-methods #'some-fn))))
      (5am:is (equal '(:superaround)
                     (c2mop::method-qualifiers
                      (car
                       (c2mop:generic-function-methods #'some-fn)))))
      (5am:is (equal '(a b)
                     (c2mop:generic-function-lambda-list #'some-fn)))
      (eval '(defmethod some-fn (a b)
              (+ a b)))
      (5am:is (= 2 (length (c2mop:generic-function-methods #'some-fn))))
      (5am:is (eql 5 (some-fn 2 3)))
      (5am:is (= 1 (hash-table-count *characterization-tests*)))
      (5am:is (= 2 (length (gethash 'some-fn *characterization-tests*))))
      (5am:is
       (equal '((5am:is (every #'eql (list 'c)
                                     (multiple-value-list (some-fn 'c))))
                (5am:is (every #'eql (list 5)
                                     (multiple-value-list (some-fn 2 3)))))
              (reverse (gethash 'some-fn *characterization-tests*)))))))
