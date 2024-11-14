<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Гультяєв Дмитро</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Завдання складається з двох частин:
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

## Варіант першої частини 5
Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
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
```
### Тестові набори та утиліти першої частини
```lisp
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
```
### Тестування першої частини
```lisp
CL-USER> (test-first-function)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
```
## Варіант другої частини 5
Написати функцію propagator-fn , яка має один ключовий параметр — функцію
comparator . propagator-fn має повернути функцію, яка при застосуванні в якості
першого аргументу mapcar разом з одним списком-аргументом робить наступне: якщо
елемент не "кращий" за попередній згідно з comparator , тоді він заміняється на
значення попереднього, тобто "кращого", елемента. Якщо ж він "кращий" за попередній
елемент згідно comparator , тоді заміна не відбувається. Функція comparator за
замовчуванням має значення #'> .

```lisp
CL-USER> (mapcar (propagator-fn) '(1 2 3))
(1 2 3)
CL-USER> (mapcar (propagator-fn) '(3 1 4 2))
(3 3 4 4)
CL-USER> (mapcar (propagator-fn :comparator #'<) '(1 2 3))
(1 1 1)

```

## Лістинг реалізації другої частини завдання
```lisp
(defun propagator-fn (&key (comparator #'>))
  (let ((prev nil))  
    (lambda (x)
      (if (or (null prev) (funcall comparator x prev))  
          (setq prev x)  
          (setq x prev))  
      x)))  
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
CL-USER> (test-second-function)
passed test 1
passed test 2
passed test 3
```

