(defun swap (lst key test)
  (cond
    ((null (rest lst))
     (values lst nil))

    ((funcall test (funcall key (first lst)) (funcall key (second lst)))
     (multiple-value-bind (rest-of-list new-flag) (swap (cons (first lst) (rest (rest lst))) key test)
       (values (cons (second lst) rest-of-list) t)))

    (t (multiple-value-bind (rest-of-list new-flag) (swap (rest lst) key test)
         (values (cons (first lst) rest-of-list) new-flag)))))


(defun sort-func (lst &key (key #'identity) (test #'>))
  (multiple-value-bind (new-list flag) (swap lst key test)
    (if flag
        (sort-func new-list :key key :test test)
        new-list)))

(defun check-first-function (name input expected)
    "Execute `my-reverse' on `input', compare result with `expected' and print
    comparison status"
    (format t "~:[FAILED~;passed~] ~a~%"
        (equal (sort-func input) expected)
        name))

(defun test-first-function ()
    (check-first-function "test 1" '(5 3 4 1 2) '(1 2 3 4 5))  
    (check-first-function "test 2" '(1 2 3 4 5) '(1 2 3 4 5)) 
    (check-first-function "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
    (check-first-function "test 4" '(2 2 3 3 1) '(1 2 2 3 3))
    (check-first-function "test 5" nil nil))



(defun propagator-fn (&key (comparator #'>))
  (let ((prev :uninitialized))  
    (lambda (x)
      (if (eq prev :uninitialized)  
          (setq prev x))
      (if (funcall comparator x prev)  
          (setq prev x)  
          (setq x prev)))))  

(defun check-second-function (name input expected)
    "Execute `my-reverse' on `input', compare result with `expected' and print
    comparison status"
    (format t "~:[FAILED~;passed~] ~a~%"
        (equal input expected)
        name))

(defun test-second-function ()
    (check-second-function "test 1" (mapcar (propagator-fn) '(1 2 3)) '(1 2 3))
    (check-second-function "test 2" (mapcar (propagator-fn) '(3 1 4 2)) '(3 3 4 4))
    (check-second-function "test 3" (mapcar (propagator-fn :comparator #'<) '(1 2 3)) '(1 1 1)))
