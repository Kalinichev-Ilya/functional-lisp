;;;; Прочитать трехмерный массив из стандартного потока ввода.
;;;; Массив может содержать элементы различных типов – целые,
;;;; вещественные, комплексные числа, строки, символы, списки,
;;;; другие массивы.
;;;; Необходимо найти сумму длин строк, не содержащих символов
;;;; #\a, #\i и #\u. Если строк в массиве нет, результат – 0.

;;;; Пример:
;;;; Дан трехмерный массив
;;;; #3A(((1.8 5 #\B "may")
;;;;     ("the" 2.25 1.3 4)
;;;;     (17 0 "force" 55))
;;;;     (("be" #C(0 1) 5.02 99)
;;;;     ("with" 1.1 (#\Y #\N) 0.87)
;;;;     ("you!" #C(2.2 3.3) 42 4.2)))
;;;;
;;;; В нем содержатся различные элементы, в том числе следующие строки:
;;;; "may" "the" "force" "be" "with" "you!"
;;;; Среди этих строк есть те, которые не содержат символов
;;;; #\a, #\i и #\u:
;;;; "the" "force" "be"
;;;; Сумма длин этих строк равна 10,
;;;; этот результат и нужно вывести в стандартный поток вывода.

(defparameter arr (read))
(defvar dimensions (array-dimensions arr))

(defvar x (car dimensions))
(defvar y (car (cdr dimensions)))
(defvar z (car (cdr (cdr dimensions))))

(defvar result 0)

(loop for i from 0 to (- x 1) do (
    loop for j from 0 to (- y 1) do (
        loop for k from 0 to (- z 1) do (
            when (typep (aref arr i j k) 'string) (
                when (eq (position #\a (aref arr i j k)) nil) (
                      when (eq (position #\i (aref arr i j k)) nil) (
                          when (eq (position #\u (aref arr i j k)) nil) (
                              setf result (+ result (length (aref arr i j k)
                    ))))))))))
(print result)