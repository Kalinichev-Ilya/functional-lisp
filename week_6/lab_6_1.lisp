;;;; Кто_звонил;Кому_звонили;Длительность остальные игнорировать
;;;; Кто начинается на +47 остальные игнорировать
;;;; Отдать кол-во исходящих минут звонившего оператора

(defparameter *vrps*
    '(
        "1101;+79119989911;122"
        " +47231114563;1102;347"
        "+47023334521;1101;134"
        "1102;+49023334521;811"
        "1102;1101;42"
        "ERR01:1234;;;0;0"
        " +390145211212; 1102; 93"
        "+47023414522;1102;753"
        "1102;+79119989911;771"
    )
)

; отдает строку перед символом ";"
(defun before-semicolon (v)
    (subseq v 0 (position #\; v)))

; отдает строку после символа ";"
(defun after-semicolon (v)
    (subseq v (1+ (position #\; v)) (length v)))

; достает номер звонящего
(defun who-number (v)
    (before-semicolon v))

; достает перфикс номера звонящего
(defun prefix (v)
    (subseq (who-number (string-trim '(#\SPACE) v)) 0 3))

; достает номер кому звонили
(defun whom-number (v)
    (before-semicolon (after-semicolon v)))

; достает кол-во минут
(defun minutes-count (v)
    (after-semicolon (after-semicolon v)))

; отдает Т для номеров начинающихся с +47
(defun from-norway (v)
    (cond
        ( ;if contains ";" char
            (numberp (search ";" v))
                (cond
                    (
                        (equal (prefix v) "+47") t
                    )
                )
        )
    )
)

; возвращает список со входящими из Норвегии
(defun filtered-calls-list (lst)
    (remove-if (complement #'from-norway) lst))

; возвращает hash -> (operator . count)
(defun operators-count(lst &optional (n 1))
    (let ((hs (make-hash-table :test 'equal)))
        (cond
            (;IF key exist than value+
               (gethash (who-number str) hs) (setf (gethash (who-number str) hs) (1+ (gethash (who-number str) hs)))
            )
            (t ;ELSE value 1
                (setf (gethash (who-number str) hs) n)
            )
        )
    )
)

; преобразует hash в assoc list для сортировок
(defun hash-to-asoc (hash)
    (let ((lst '()))
        (maphash
            #'(lambda (key value) (push (cons key value) lst))
        hash)
    lst)
)

; сортировка hash по убыванию
(defun sorted-assoc-lst (hash)
    (sort (hash-to-asoc hash) #'> :key #'cdr)))

; отдает номер оператора с большим кол-ом звонков
(defun max-count-operator (hash)
    (first
        (first (sorted-assoc-lst hash))))

; оператор которому звонили из Норвегии большее кол-во раз
(max-count-operator
    (operators-count (filtered-calls-list *vrps*)))

; возвращает T есть звонил нужный оператор
(defun operator-caller (v)
    (cond
        ( ;IF contains ";" char
            (numberp (search ";" v))
                (cond
                    ( ;AND who called is operator
                        (string= (who-number v)
                            (max-count-operator
                                (operators-count (filtered-calls-list *vrps*)))) t
                    )
                )
        )
    )
)

; фильтрует список, оставляет звонки нужного оператора
(defun outgoing-call-lst (lst)
    (remove-if (complement #'operator-caller) lst))

; ответ
(print (reduce (minutes-count (outgoing-call-lst *vrps*))))
