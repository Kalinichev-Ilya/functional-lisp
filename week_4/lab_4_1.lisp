(defun count-of-odds (lst)
    "Checking that all elemets in list is odd."
         (cond
             ( ;IF null return 0 and quit
                 (null lst) 0
             )
             ( ;IF number
                 (numberp lst)
                     (cond
                         ( ;AND even, return 0
                             (evenp lst) 0
                         )
                         ( ;AND odd, return 1
                             (oddp lst) 1
                         )
                     )
             )
             ( ;ELSE recursively call and split for first element
               ;and tail
                 (listp lst) (+ (count-of-odds (car lst)) (count-of-odds (cdr lst)))
             )
         )
)

(defun sum-odds (lst)
    "Sum numbers if list contains only odd elemets."
    (defvar result-list nil)
    (defvar index 1)
    (dolist (i lst)
        (format t "~C--" #\linefeed)
        (cond
            ( ;IF list than recursively call
                (listp i) (setf (nth (- index 1) lst) (sum-odds i))
            )
            ( ;IF number is last and list of odd elements
                (and (= index (list-length lst)) (= (count-of-odds lst) (list-length lst)))
                    (print i) ; не заходит :(
                    (apply '+ lst)
            )
            (t ;ELSE add in list
                (push i result-list)
            )
        )
        (incf index)
    )
    (values result-list)
)
