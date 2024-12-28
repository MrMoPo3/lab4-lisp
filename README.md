<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Левчук Іван Володимирович група КВ-12<p>
<p align="right">Рік: 2024<p>

## Загальне завдання
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де це
доречно);
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

## Варіант першої частини - 6
Алгоритм сортування вставкою №1 (з лінійним пошуком зліва) за незменшенням.

## Лістинг реалізації першої частини завдання

```lisp
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

```

## Лістинг функції insert-imperative

```lisp
(defun insert-imperative (element sorted-vector vector-length)
  (let ((i 0))
    (loop while (< i vector-length)
          while (< (aref sorted-vector i) element)
          do (incf i))
    (loop for j from (1- vector-length) downto i
          do (setf (aref sorted-vector (1+ j)) (aref sorted-vector j)))
    (setf (aref sorted-vector i) element)))

(defun insert-sort-imperative (unsorted-list)
  (let* ((list-length (length unsorted-list))
         (sorted-vector (make-array list-length :initial-element nil))
         (current-length 0))
    (dolist (element unsorted-list)
      (insert-imperative element sorted-vector current-length)
      (incf current-length))
    (subseq sorted-vector 0 current-length)))

```

### Тестові набори першої частини

```lisp
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
```
### Тестування першої частини

```lisp
Start testing functional insert-sort function
test 1 passed! Expected: (0 1 5 10 20 50 100) Obtained: (0 1 5 10 20 50 100)
test 2 passed! Expected: (-3 0 1 5 7 8 15 23 42) Obtained: (-3 0 1 5 7 8 15 23 42)
test 3 passed! Expected: (3 14 26 159 535 897) Obtained: (3 14 26 159 535 897)
test 4  passed! Expected: (5 4 3 2 1) Obtained: (5 4 3 2 1)
test 5  passed! Expected: (kiwi apple banana) Obtained: (kiwi apple banana)
test 6  passed! Expected: (banana apple kiwi) Obtained: (banana apple kiwi)
test 7  passed! Expected: ((1 . 2) (3 . 4) (5 . 6)) Obtained: ((1 . 2) (3 . 4)(5 . 6))
test 8  passed! Expected: ((5 . 6) (3 . 4) (1 . 2)) Obtained: ((5 . 6) (3 . 4)(1 . 2))
test 9  passed! Expected: ((1 . 2) (3 . 4) (5 . 6)) Obtained: ((1 . 2) (3 . 4)(5 . 6))
test 10  passed! Expected: ((5 . 6) (3 . 4) (1 . 2)) Obtained: ((5 . 6) (3 . 4)(1 . 2))
End
```

## Варіант другої частини - 2
Написати функцію add-prev-reducer , яка має один ключовий параметр — функцію
transform . add-prev-reducer має повернути функцію, яка при застосуванні в якості
першого аргументу reduce робить наступне: кожен елемент списку-аргументу reduce
перетворюється на точкову пару, де в комірці CAR знаходиться значення поточного
елемента, а в комірці CDR знаходиться значення попереднього елемента списку (тобто
того, що знаходиться "зліва"). Якщо функція transform передана, тоді значення
поточного і попереднього елементів, що потраплять у результат, мають бути змінені
згідно transform . Обмеження, які накладаються на використання функції-результату

add-prev-reducer при передачі у reduce визначаються розробником (тобто,
наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів
функції reduce from-end та initial-value ). transform має виконатись мінімальну
кількість разів.
```lisp
CL-USER> (reduce (add-prev-reducer)

'(1 2 3)
:from-end ...
:initial-value ...)

((1 . NIL) (2 . 1) (3 . 2))
CL-USER> (reduce (add-prev-reducer :transform #'1+)

'(1 2 3)
:from-end ...
:initial-value ...)

((2 . NIL) (3 . 2) (4 . 3))
```

## Лістинг реалізації другої частини завдання
```lisp
(defun add-prev-reducer (transform)

  (lambda (acc elem)
    (let ((processed-elem (funcall transform elem))
          (prev (if (null acc) nil (caar acc)))) 
      (cons (cons processed-elem prev) acc)))) 
```

### Тестові набори та утиліти другої частини 

```lisp
(defun check-add-prev-reducer (test-name input expected transform)
  "Виконує редукцію з add-prev-reducer, порівнює результат з очікуваним і виводить статус порівняння"
  (let ((result (nreverse (reduce (add-prev-reducer transform) input :initial-value nil))))
    (format t "~:[~a failed! Expected: ~a Obtained: ~a~;~a passed! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            test-name expected result)))

(defun test-add-prev-reducer ()
  (format t "Start testing add-prev-reducer function~%")
  ;; Тест 1: Простий список з трансформацією (- x -1)
  (check-add-prev-reducer "test 1" '(1 2 3) '((1 . NIL) (2 . 1) (3 . 2)) 
                          (lambda (x) (+ x 0)))

  ;; Тест 2: Список з від'ємними числами
  (check-add-prev-reducer "test 2" '(1 2 3) '((2 . NIL) (3 . 2) (4 . 3)) 
                          (lambda (x) (+ x 1)))
```
### Тестування другої частини 

```lisp
Output:

Start testing add-prev-reducer function
test 1 passed! Expected: ((1) (2 . 1) (3 . 2)) Obtained: ((1) (2 . 1) (3 . 2))
test 2 passed! Expected: ((2) (3 . 2) (4 . 3)) Obtained: ((2) (3 . 2) (4 . 3))
End
```
