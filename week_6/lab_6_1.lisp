; функции обработки Hash start----
(defun set% (h key val &rest other-pairs)
  (let ((pairs (append (list key val) other-pairs)) k v)
      (hash-table-add-list h pairs)))

; считает кол-во одинаковых элементов в hash
(defun hash++ (h key &optional (n 1))
    (let ((val (gethash key h)))
        (cond
            ((or (null val) (not (numberp val)))
                (set% h key n))
            (t (set% h key (+ val n))))
        (gethash key h)))

; добавляет в hash лист
(defun hash-table-add-list (h lst &aux k v)
    (loop
        (if (null lst) (return))
        (setf k (pop lst))
        (if (null lst) (return))
        (setf v (pop lst))
        (setf (gethash k h) v))
    h)

; формирует hash
(defun hash-table-key-value-pairs (h)
    (let (pairs)
        (maphash #'(lambda (k v)
            (push (list k v) pairs)) h)
        (nreverse pairs)))
; ----end

;;;; Кто_звонил;Кому_звонили;Длительность остальные игнорировать
;;;; Номер из 4х символов;
;;;; Кому начинается на +49 и +39

(defparameter *vrps*
    '(
        "1101;+79119989911;122"
        "+49231114563;1102;347"
        "1101;+420023334521;134"
        "1102;+49023334521;811"
        "1102;+49023334521;81"
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

; маска на номер оператора
(defun is-operator-phone (v)
    (and (>= (length v) 4)
        (char= #\; (elt v 4))
        (digit-char-p (elt v 0))
        (digit-char-p (elt v 1))
        (digit-char-p (elt v 2))
        (digit-char-p (elt v 3))))

; отдает Т для номеров начинающихся с +49 и +39
(defun is-liquid-recipient (v)
    (or
        (equal (subseq (recipient-number v) 0 3) "+49")
        (equal (subseq (recipient-number v) 0 3) "+39")))

; достает номер оператора
(defun operator-number (v)
    (subseq v 0 (position #\; v)))

; достает номер абонента
(defun recipient-number (v)
    (subseq v (1+ (position #\; v))))

; очищенный список
; TODO вырезать номер абонента
(defun filtered-calls-list (lst)
    (remove-if
        (complement #'is-operator-phone)
            (remove-if
                (complement #'is-liquid-recipient) lst)))

; TODO сделать hash (номер оператора . кол-во минут)
; TODO сложить кол-во минут по номеру оператора
; TODO отсортировать по убыванию
; TODO вернуть номер оператора из нулевой позиции