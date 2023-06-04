(in-package :autoCLaracterization)

(5am:def-suite :defrecfun :in :autoCLaracterization)

(5am:in-suite :defrecfun)

(5am:test :fib
  (with-defrecfun-preamble (fibdefun) (defun fib (n)
                                        (if (<= n 1)
                                            n
                                            (+ (fib (- n 1))
                                               (fib (- n 2)))))
    (load-defrecfun fibdefun :test '#'=)
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

(5am:test :box
  (with-defrecfun-preamble (rectfun) (defun box (width &optional
                                                         (depth width)
                                                         (height width))
                                       (list width depth height))
    (load-defrecfun rectfun :test '#'equal)
    (5am:is (equal '(100 100 100)
                   (box 100)))
    (5am:is (equal '(100 200 100)
                   (box 100 200)))
    (5am:is (equal '(100 200 300)
                   (box 100 200 300)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 3 (length (gethash 'box *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'equal (list (list 100 100 100))
                                     (multiple-value-list (box 100))))
              (5am:is (every #'equal (list (list 100 200 100))
                                     (multiple-value-list (box 100 200))))
              (5am:is (every #'equal (list (list 100 200 300))
                                     (multiple-value-list (box 100 200 300)))))
            (reverse (gethash 'box *characterization-tests*))))))
