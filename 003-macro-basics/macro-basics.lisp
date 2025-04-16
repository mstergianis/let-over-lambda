(defmacro sleep-units% (value unit)
  `(sleep
    ,(* value
        (case unit
          ((s) 1)
          ((m) 60)
          ((h) 3600)
          ((d) 86400)
          ((ms) 1/1000)
          ((us) 1/1000000)))))

(sleep-units% 100 s)
(SLEEP 100)

(defmacro nlet (n letargs &rest body)
`(labels ((,n ,(mapcar #'car letargs)
            ,@body))
   (,n ,@(mapcar #'cadr letargs))))
