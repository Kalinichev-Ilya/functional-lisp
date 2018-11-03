;;;; Кто_звонил;Кому_звонили;Длительность остальные игнорировать
;;;; Номер из 4х символов;
;;;; Кому начинается на +49 и +39

(defparameter *vrps* '(
    "1101;+79119989911;122"
    "+49231114563;1102;347"
    "1101;+420023334521;134"
    "1102;+49023334521;811"
    "ERR01:1234;;;0;0"
    "1101;+390145211212;93"
    "1101;+49023334521;756"
    )
)

;;;; оставить в списке только записи где звонил внутренний номер
;;;; - remove-if
;;;; оставить в списке только запись где звонили в Германию и Испанию
;;;; - remove-if
;;;; подсчитать кол-во минут для каждого исходящего номера
;;;; - reduce
;;;; вывести внустренний номер сотрудник который звонил больше всего

; проверка на содержание цифры
(defun is-liquid-number (c)
    (and (find c "01"
        :test #'string-equal) t))

; проверка на номер оператора
(defun is-operator-phone (v)
    (print v)
    (and (>= (length v) 4)
        (char= #\; (elt v 4))
        (is-liquid-number (elt v 0))
        (is-liquid-number (elt v 1))
        (is-liquid-number (elt v 2))
        (is-liquid-number (elt v 3))))

; проверка на нужный номер
(defun is-liquid-recipient (number prefix)
        (equal (subseq number 0 3) prefix))

# делит строку по ;
(defun number-before-semicolon (v)
    (subseq v (1+ (position #\; v))))

(defun get-operators-list (lst)
    (mapcar
        #'number-before-semicolon
    (remove-if
        (complement #'is-operator-phone)
        lst)))

