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

; формирует hash
(defun hash-table-key-value-pairs (h)
    (let (pairs)
        (maphash #'(lambda (k v)
            (push (list k v) pairs)) h)
        (nreverse pairs)))

; добавляет в hash лист
(defun hash-table-add-list (h lst &aux k v)
    (loop
        (if (null lst) (return))
        (setf k (pop lst))
        (if (null lst) (return))
        (setf v (pop lst))
        (setf (gethash k h) v))
    h)
; ----end

; список знаков
(defparameter *vrps*
    '(
        "ао365;78" "0245ок;43"
        "с227на;69" "а1234;78"
         "ек201;69" "с304вв;70"
         "у333ух;71" "о001оо;78"
         "ху045;78" "а144нс;78"
         "а144нс;78" "е4433;98"
         "002cd1;178" "а144нс;78"
         "е042кх;777" "е043кх;777"
         "3340но;150" "е044кх;777"
    )
)

; проверяем на нужные буквы
(defun vrp-letter-p (c)
    (and (find c "авекмнорстух"
        :test #'char-equal) t))

; маска на нужный формат номера например с227на;69
(defun private-vrp-p (v)
    (and (>= (length v) 9)
            (char= #\; (elt v 6))
            (vrp-letter-p (elt v 0))
            (digit-char-p (elt v 1))
            (digit-char-p (elt v 2))
            (digit-char-p (elt v 3))
            (vrp-letter-p (elt v 4))
            (vrp-letter-p (elt v 5))))

; вернет код региона
(defun region-from-vrp (v)
    (subseq v (1+ (position #\; v))))

; результирующая функция
(defun get-region-list (lst)
    (sort
        (hash-table-key-value-pairs
            (reduce
                #'(lambda (h e) (hash++ h (region-from-vrp e)) h)
                (remove-duplicates
                    (remove-if
                        (complement #'private-vrp-p) lst)
                    :test #'string-equal)
                :initial-value (make-hash-table :test #'equal)))
        #'> :key #'second))
