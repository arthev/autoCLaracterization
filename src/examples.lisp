(in-package :autoCLaracterization)

(defrecfun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
