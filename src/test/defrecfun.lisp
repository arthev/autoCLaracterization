(in-package :autoCLaracterization)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(5am:def-suite :defrecfun :in :autoCLaracterization)

(5am:in-suite :defrecfun)

(5am:test :fib ; tests required
  (with-defrec-preamble (fibdefun)
                        (defun fib (n)
                             (if (<= n 2)
                                 1
                                 (+ (fib (- n 1))
                                    (fib (- n 2)))))
    (load-defrec fibdefun :test '#'=)
    (5am:is (=   1 (fib 1)))
    (5am:is (=  8 (fib 6)))
    (5am:is (= 610 (fib 15)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 3 (length (gethash 'fib *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'= (list 1)   (multiple-value-list (fib  1))))
              (5am:is (every #'= (list 8)   (multiple-value-list (fib  6))))
              (5am:is (every #'= (list 610) (multiple-value-list (fib 15)))))
            (reverse (gethash 'fib *characterization-tests*))))))

(5am:test :box ; tests optional
  (with-defrec-preamble (rectfun) (defun box (width &optional
                                                      (depth width)
                                                      (height width))
                                    (list width depth height))
    (load-defrec rectfun :test '#'equal)
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
  (with-defrec-preamble (silly-listfun) (defun silly-list (&rest rest) rest)
    (load-defrec silly-listfun :test '#'equal)
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
  (with-defrec-preamble (sillier-listfun)
                           (defun sillier-list (&rest rest)
                             (values-list rest))
    (load-defrec sillier-listfun)
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
  (with-defrec-preamble (silliest-listfun)
                           (defun silliest-list
                               (&key a b)
                             (list :a a :b b))
    (load-defrec silliest-listfun :test '#'equal)
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
  (with-defrec-preamble (avg-auxfun)
                           (defun avg-aux (&rest nums
                                           &aux (avg
                                                 (/ (reduce #'+ nums)
                                                    (length nums))))

                             avg)
    (load-defrec avg-auxfun :test '#'=)
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
  (with-defrec-preamble (supersilly-listfun)
                           (defun supersilly-list (a &optional b &rest r &key k &aux (aux :aux))
                             (list :a a :b b :r r :k k :aux aux))
    (load-defrec supersilly-listfun :test '#'equal)
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
  (with-defrec-preamble (partialfun)
                           (defun partial (&rest nums)
                             (if (null nums)
                                 (values 0 (list 0))
                                 (multiple-value-bind (sum partials)
                                     (apply #'partial (cdr nums))
                                   (let ((new-sum (+ (car nums) sum)))
                                     (values new-sum (cons new-sum partials))))))
    (load-defrec partialfun
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

(5am:test :all-strategy
  (with-defrec-preamble (fibdefun)
                        (defun fib (n)
                          (if (<= n 2)
                              1
                              (+ (fib (- n 1))
                                 (fib (- n 2)))))
    (load-defrec fibdefun :test '#'= :strategy :all)
    (5am:is (=  21 (fib 8)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 8 (length (gethash 'fib *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'= (list  1)   (multiple-value-list (fib  2))))
              (5am:is (every #'= (list  1)   (multiple-value-list (fib  1))))
              (5am:is (every #'= (list  2)   (multiple-value-list (fib  3))))
              (5am:is (every #'= (list  3)   (multiple-value-list (fib  4))))
              (5am:is (every #'= (list  5)   (multiple-value-list (fib  5))))
              (5am:is (every #'= (list  8)   (multiple-value-list (fib  6))))
              (5am:is (every #'= (list 13)   (multiple-value-list (fib  7))))
              (5am:is (every #'= (list 21)   (multiple-value-list (fib  8)))))
            (reverse (gethash 'fib *characterization-tests*))))))

(5am:test :outer-only-strategy
  (with-defrec-preamble (fibdefun)
                        (defun fib (n)
                          (if (<= n 2)
                              1
                              (+ (fib (- n 1))
                                 (fib (- n 2)))))
    (load-defrec fibdefun :test '#'= :strategy :all)
    (load-defrec '(defun square-fib (n)
                   (* (fib n) (fib n)))
                 :test '#'=
                 :strategy :outer-only)
    (5am:is (=  441 (square-fib 8)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 1 (length (gethash 'square-fib *characterization-tests*))))
    (5am:is (= 0 (length (gethash 'fib *characterization-tests*))))
    (5am:is
     (equal '((5am:is (every #'= (list  441)
                                 (multiple-value-list (square-fib 8)))))
            (reverse (gethash 'square-fib *characterization-tests*))))
    (5am:is (= 21 (fib 8)))
    (5am:is (= 2 (hash-table-count *characterization-tests*)))
    (5am:is (= 1 (length (gethash 'square-fib *characterization-tests*))))
    (5am:is (= 8 (length (gethash 'fib *characterization-tests*))))))


(5am:test :mutually-recursive-entry-only-strategy
  (with-defrec-preamble (mrevenfun)
                        (defun mreven-p (n)
                          (if (zerop n)
                              t
                              (mrodd-p (1- n))))
    (load-defrec mrevenfun)
    (load-defrec '(defun mrodd-p (n)
                   (if (zerop n)
                       nil
                       (mreven-p (1- n)))))
    (5am:is (eq t
                (mreven-p 10)))
    (5am:is (= 2 (hash-table-count *characterization-tests*)))
    (5am:is (= 1 (length (gethash 'mreven-p *characterization-tests*))))
    (5am:is (= 1 (length (gethash 'mrodd-p *characterization-tests*))))))


(5am:test :records-desired-conditions
  (with-defrec-preamble (somefun)
                        (defun some-fn ()
                          (error "whatever"))
    (load-defrec somefun)
    (5am:signals simple-error
      (some-fn))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 1 (length (gethash 'some-fn *characterization-tests*))))
    (5am:is
     (equal '((5am:signals simple-error
                (some-fn)))
            (reverse (gethash 'some-fn *characterization-tests*))))))

(5am:test :nil-means-no-condition-record
  (with-defrec-preamble (somefun)
                        (defun some-fn ()
                          (error "whatever"))
    (let ((*record-conditions* nil))
      (load-defrec somefun)
      (5am:signals simple-error
        (some-fn))
      (5am:is (= 0 (hash-table-count *characterization-tests*)))
      (5am:is (= 0 (length (gethash 'some-fn *characterization-tests*)))))))

(5am:test :records-specific-conditions
  (with-defrec-preamble (somefun)
                        (defun some-fn (a)
                          (if a
                              (+ 2 'a)
                              (error "whatever")))
    (let ((*record-conditions* 'type-error))
      (load-defrec somefun)
      (5am:signals simple-error
        (some-fn nil))
      (5am:is (= 0 (hash-table-count *characterization-tests*)))
      (5am:is (= 0 (length (gethash 'some-fn *characterization-tests*))))
      (5am:signals
          #+allegro type-error
          #+sbcl simple-type-error
        (some-fn t))
      (5am:is (= 1 (hash-table-count *characterization-tests*)))
      (5am:is (= 1 (length (gethash 'some-fn *characterization-tests*))))
      (5am:is
       (equal '((5am:signals
                    #+allegro type-error
                    #+sbcl simple-type-error
                  (some-fn t)))
              (reverse (gethash 'some-fn *characterization-tests*)))))))

(5am:test :records-conditions-and-values-if-local-restarts
  (with-defrec-preamble (somefun)
                        (defun some-fn ()
                          (restart-case
                              (+ 2 'a)
                            (move-along ()
                              0)))
    (load-defrec somefun)
    (compile (eval '(defun move-along-invoker (fn)
                     (handler-bind ((error
                                      (lambda (e)
                                        (invoke-restart 'move-along))))
                       (funcall fn)))))
    (5am:is (= 0 (move-along-invoker #'some-fn)))
    (5am:is (= 1 (hash-table-count *characterization-tests*)))
    (5am:is (= 2 (length (gethash 'some-fn *characterization-tests*))))
    (5am:is
     (equal '((5am:signals
                  #+allegro type-error
                  #+sbcl simple-type-error
                (some-fn))
              (5am:is (every #'eql (list 0)
                                   (multiple-value-list (some-fn)))))
            (reverse (gethash 'some-fn *characterization-tests*))))))
