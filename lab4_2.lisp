(defun add-prev-reducer (transform)
  (lambda (acc elem)
    (let ((processed-elem (funcall transform elem))
          (prev (if (null acc) nil (caar acc))))
      (cons (cons processed-elem prev) acc))))

(defun check-add-prev-reducer (test-name input expected transform)
  "Виконує редукцію з add-prev-reducer, порівнює результат з очікуваним і виводить статус порівняння"
  (let ((result (nreverse (reduce (add-prev-reducer transform) input :initial-value nil))))
    (format t "~:[~a failed! Expected: ~a Obtained: ~a~;~a passed! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            test-name expected result)))

(defun test-add-prev-reducer ()
  (format t "Start testing add-prev-reducer function~%")
  ;; Тест 1: Простий список з трансформацією (+ x 0)
  (check-add-prev-reducer "test 1" '(1 2 3) '((1 . NIL) (2 . 1) (3 . 2))
                          (lambda (x) (+ x 0)))

  ;; Тест 2: Список з від'ємними числами
  (check-add-prev-reducer "test 2" '(1 2 3) '((2 . NIL) (3 . 2) (4 . 3))
                          (lambda (x) (+ x 1))))

;; Виклик тестів
(test-add-prev-reducer)

