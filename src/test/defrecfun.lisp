(in-package :autoCLaracterization)

(5am:def-suite :defrecfun :in :autoCLaracterization)

(5am:in-suite :defrecfun)

(5am:test :fib ; tests required
  (with-defrecfun-preamble (fibdefun)
                           (defun fib (n)
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

(5am:test :box ; tests optional
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

(5am:test :silly-list ; tests rest
  (with-defrecfun-preamble (silly-listfun) (defun silly-list (&rest rest) rest)
    (load-defrecfun silly-listfun :test '#'equal)
    (5am:is (equal '(a)
                   (silly-list 'a)))
    (5am:is (equal '(a b c d e f g)
                   (silly-list 'a 'b 'c 'd 'e 'f 'g)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 2 (length (gethash 'silly-list *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'equal (list (list 'a))
                                     (multiple-value-list
                                      (silly-list 'a))))
              (5am:is (every #'equal (list (list 'a 'b 'c 'd 'e 'f 'g))
                                     (multiple-value-list
                                      (silly-list 'a 'b 'c 'd 'e 'f 'g)))))
            (reverse (gethash 'silly-list *characterization-tests*))))))

(5am:test :sillier-list ; tests values correctly handled
  (with-defrecfun-preamble (sillier-listfun)
                           (defun sillier-list (&rest rest)
                             (values-list rest))
    (load-defrecfun sillier-listfun)
    (5am:is (= 1
               (sillier-list 1)))
    (5am:is (= 10
               (sillier-list 10 9 8 7 6 5 4 3 2 1 0 -1)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 2 (length (gethash 'sillier-list *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'eql (list 1)
                                   (multiple-value-list
                                    (sillier-list 1))))
              (5am:is (every #'eql (list 10 9 8 7 6 5 4 3 2 1 0 -1)
                                   (multiple-value-list
                                    (sillier-list 10 9 8 7 6 5 4 3 2 1 0 -1)))))
            (reverse (gethash 'sillier-list *characterization-tests*))))))

(5am:test :silliest-list ; tests key
  (with-defrecfun-preamble (silliest-listfun)
                           (defun silliest-list
                               (&key a b)
                             (list :a a :b b))
    (load-defrecfun silliest-listfun :test '#'equal)
    (5am:is (equal '(:a nil :b nil)
                   (silliest-list)))
    (5am:is (equal '(:a 1 :b nil)
                   (silliest-list :a 1)))
    (5am:is (equal '(:a nil :b 2)
                   (silliest-list :b 2)))
    (5am:is (equal '(:a 1 :b 2)
                   (silliest-list :a 1 :b 2)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 4 (length (gethash 'silliest-list *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'equal (list (list :a nil :b nil))
                                     (multiple-value-list
                                      (silliest-list))))
              (5am:is (every #'equal (list (list :a 1 :b nil))
                                     (multiple-value-list
                                      (silliest-list :a 1))))
              (5am:is (every #'equal (list (list :a nil :b 2))
                                     (multiple-value-list
                                      (silliest-list :b 2))))
              (5am:is (every #'equal (list (list :a 1 :b 2))
                                     (multiple-value-list
                                      (silliest-list :a 1 :b 2)))))
            (reverse (gethash 'silliest-list *characterization-tests*))))))



(5am:test :avg-aux ; tests aux
  (with-defrecfun-preamble (avg-auxfun)
                           (defun avg-aux (&rest nums
                                           &aux (avg
                                                 (/ (reduce #'+ nums)
                                                    (length nums))))

                             avg)
    (load-defrecfun avg-auxfun :test '#'=)
    (5am:is (= 1
               (avg-aux 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
    (5am:is (= 2
               (avg-aux 1 3 0 4 -1 5 -2 6 -3 7 -4 8 -5 9)))
    (5am:is (= 3/2
               (avg-aux 1 2)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 3 (length (gethash 'avg-aux *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'= (list 1)
                                 (multiple-value-list
                                  (avg-aux 1 1 1 1 1 1 1 1 1 1 1 1 1
                                           1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
              (5am:is (every #'= (list 2)
                                 (multiple-value-list
                                  (avg-aux 1 3 0 4 -1 5 -2 6 -3 7 -4 8 -5 9))))
              (5am:is (every #'= (list 3/2)
                                 (multiple-value-list
                                  (avg-aux 1 2)))))
            (reverse (gethash 'avg-aux *characterization-tests*))))))

(5am:test :supersilly-list ; tests all &s combine correctly
  (with-defrecfun-preamble (supersilly-listfun)
                           (defun supersilly-list (a &optional b &rest r &key k &aux (aux :aux))
                             (list :a a :b b :r r :k k :aux aux))
    (load-defrecfun supersilly-listfun :test '#'equal)
    (5am:is (equal '(:a 1 :b nil :r nil :k nil :aux :aux)
                   (supersilly-list 1)))
    (5am:is (equal '(:a 1 :b 2 :r nil :k nil :aux :aux)
                   (supersilly-list 1 2)))
    (5am:is (equal '(:a 1 :b 2 :r (:k 3) :k 3 :aux :aux)
                   (supersilly-list 1 2 :k 3)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 3 (length (gethash 'supersilly-list *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'equal (list (list :a 1 :b nil :r nil
                                                 :k nil :aux :aux))
                                     (multiple-value-list
                                      (supersilly-list 1))))
              (5am:is (every #'equal (list (list :a 1 :b 2 :r nil
                                                 :k nil :aux :aux))
                                     (multiple-value-list
                                      (supersilly-list 1 2))))
              (5am:is (every #'equal (list (list :a 1 :b 2 :r (list :k 3)
                                                 :k 3 :aux :aux))
                                     (multiple-value-list
                                      (supersilly-list 1 2 :k 3)))))
            (reverse (gethash 'supersilly-list *characterization-tests*))))))

(5am:test :partials ; tests custom-test generates expected form
  (with-defrecfun-preamble (partialfun)
                           (defun partial (&rest nums)
                             (if (null nums)
                                 (values 0 (list 0))
                                 (multiple-value-bind (sum partials)
                                     (apply #'partial (cdr nums))
                                   (let ((new-sum (+ (car nums) sum)))
                                     (values new-sum (cons new-sum partials))))))
    (load-defrecfun partialfun
                    :custom-test '(lambda (x y)
                                   (and (= (car x) (car y))
                                        (equal (cadr x) (cadr y)))))
    (5am:is (= 5
               (partial 1 2 2)))
    (5am:is (equal '(5 4 2 0)
                   (nth-value 1 (partial 1 2 2))))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 1 (length (gethash 'partial *characterization-tests*))))
    (5am:is
     (equal '((5am:is (funcall (lambda (x y)
                                 (and (= (car x) (car y))
                                      (equal (cadr x) (cadr y))))
                       (list 5 (list 5 4 2 0))
                       (multiple-value-list
                        (partial 1 2 2)))))
            (reverse (gethash 'partial *characterization-tests*))))))