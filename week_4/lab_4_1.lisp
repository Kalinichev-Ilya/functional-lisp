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
               t
                 (listp lst) (+ (count-of-odds (car lst)) (count-of-odds (cdr lst)))
             )
         )
)


;  иду по списку
;    если эл-т последний и список из нечетных чисел
;      сложил эл-ты списка
;      заменяю результатом этот список
;    если элемент список, вызываю эту же функцию
;  возвращаю измененный список

(defun sum-all-odds (lst)
    (let ((index 1) (res-list nil))
            (dolist (i lst)
                (cond
                    ((and (= index (list-length lst)) (all-odds lst))
                        (format t "summing ~S~% and result ~S~% and new list ~S~%"
                            lst (push (apply '+ lst) res-list) res-list)
                        (incf index))
                    ((listp i)
                        (sum-all-odds i)
                        (incf index))
                    (t (push i res-list)
                       (incf index)))))
    (values res-list))

        (setf (nth (- index 1) lst)


(defun fib-tail (n &optional (prev 1) (next 1))
    (if (< n 2)
        next
        (fib-tail (- n 1) next (+ prev next))))

(defun sum-all-odds (lst &optional (index 1) (next 0))
    (if ))


defun split (x y)
  (cond
    ( ;IF: first element in list is nil
      (EQ (car x) nil)
        x ;RETURN the list
    )
    ( ;ELSE IF: first element is 'FOO'
      (EQ (car x) 'FOO)
        (cons (reverse y ) (cons (cdr x) nil))
    )
    ( ;ELSE: recursively call split but pass the rest of x and
      ;prepend y with the head of x
      t
        (split (cdr x) (cons (car x) y))
    )
  ) ;END cond
)

(defun sum-all-odds (lst)
    (let ((index 1) (res-list nil))
        (dolist (i lst)
            (cond
                (
                    (listp i)
                        (sum-all-odds i)
                (
                    (evenp i)
                        (return)
                (t
                    (push i res-list)


(defun sum-all-odds (lst)
   (cond
       (
           (null lst) nil)
       (
           (listp lst) (sum-all-odds (car lst))
       (t
           (+
               (count-atoms (car my-list)) (count-atoms(cdr my-list))
           )
       )
   )
)
