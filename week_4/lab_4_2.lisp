(defun depth-first-search (lst &optional (level 0))
    "Bypasses the graph and recursively adds only multiples of seven values to odd levels."
    (cond
        ( ;IF null return 0 and quit
            (null lst)
                (decf level)
                0
        )
        ( ;IF number
            (numberp lst)
                (cond
                    ( ;AND odd level AND multiple seven, return num
                        (AND (oddp level) (= (rem lst 7) 0))
                            (decf level)
                            lst
                    )
                    (t ;ELSE return 0
                        (decf level)
                        0
                    )
                )
        )
        ( ;ELSE recursively call, AND sum results
            (listp lst)
                (decf level)
                (+ (depth-first-search (car lst) level) (depth-first-search (cdr lst) (incf level)))
        )
    )
)