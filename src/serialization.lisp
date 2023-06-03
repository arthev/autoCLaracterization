(in-package :autoCLaracterization)

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
  :todo)
