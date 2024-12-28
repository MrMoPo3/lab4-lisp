(defun insert-functional (element sorted-list key test)
  (cond
    ((null sorted-list) (list element))
    ((funcall test (funcall key element) (funcall key (car sorted-list))) 
     (cons element sorted-list))
    (t (cons (car sorted-list) (insert-functional element (cdr sorted-list) key test)))))

(defun insert-sort-functional (unsorted-list &key (key #'identity) (test #'<))
  (reduce (lambda (sorted element)
            (insert-functional element sorted key test))
          unsorted-list
          :initial-value nil))
          
(defun check-insertion-functional (test-name input expected &key (key #'identity) (test #'<))
  "Execute functional insert-sort function on input with key and test, compare result with expected and print comparison status"
  (let ((result (insert-sort-functional input :key key :test test)))
    (format t "~:[~a failed! Expected: ~a Obtained: ~a~;~a passed! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            test-name expected result)))

(defun test-insertion-functional ()

  (format t "Start testing functional insert-sort function~%")
  ;; Test default behavior (ascending order, identity key)
  (check-insertion-functional "test 1" '(100 50 20 10 5 1 0) '(0 1 5 10 20 50 100))
  (check-insertion-functional "test 2" '(15 42 8 -3 0 23 7 5 1) '(-3 0 1 5 7 8 15 23 42))
  (check-insertion-functional "test 3" '(3 14 159 26 535 897) '(3 14 26 159 535 897))

  ;; Test descending order
  (check-insertion-functional "test 4 " '(1 2 3 4 5) '(5 4 3 2 1) :test #'>)

  ;; Test with a key function (e.g., length of strings)
  (check-insertion-functional "test 5 " '("apple" "kiwi" "banana") '("kiwi" "apple" "banana") :key #'length)

  ;; Test with a key function and custom test (descending length)
  (check-insertion-functional "test 6 " '("apple" "kiwi" "banana") '("banana" "apple" "kiwi") :key #'length :test #'>)

  ;; Tests with list pairs
  (check-insertion-functional "test 7 " '((3 . 4) (1 . 2) (5 . 6)) '((1 . 2) (3 . 4) (5 . 6)) :key #'car)
  (check-insertion-functional "test 8 " '((3 . 4) (1 . 2) (5 . 6)) '((5 . 6) (3 . 4) (1 . 2)) :key #'car :test #'>)
  (check-insertion-functional "test 9 " '((3 . 4) (1 . 2) (5 . 6)) '((1 . 2) (3 . 4) (5 . 6)) :key #'cdr)
  (check-insertion-functional "test 10 " '((3 . 4) (1 . 2) (5 . 6)) '((5 . 6) (3 . 4) (1 . 2)) :key #'cdr :test #'>)


  (format t "End~%"))
(test-insertion-functional)
