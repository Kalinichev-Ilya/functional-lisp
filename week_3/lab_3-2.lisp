;;;; Прочитать трехмерный массив, содержащий строки,
;;;; из стандартного потока ввода. Необходимо определить,
;;;; какая строка встречается в массиве чаще всего,
;;;; и вывести на экран количество вхождений.
;;;;
;;;; Пример:
;;;; Дан трехмерный массив
;;;; #3A((("R" "Lisp" "Python" "perl")
;;;;     ("C" "Lisp" "Pascal" "D"))
;;;;     (("SQL" "Java" "Pascal" "Lisp")
;;;;     ("Nemerle" "R" "Julia" "piet"))
;;;;     (("C++" "Oberon" "C" "Pascal")
;;;;     ("Lisp" "Python" "Ruby" "FORTRAN")))
;;;;
;;;; Чаще всего в нем встречается строка "Lisp" –
;;;; количество вхождений этой строки равно 4,
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
                if (string= (aref arr i j k) "Lisp")
                    (setf result (incf result)
                    )))))
result