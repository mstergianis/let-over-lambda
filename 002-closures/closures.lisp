(function (lambda (x) (+ 1 x)))


(lambda (x) (+ 1 x))

((lambda (x) (+ 1 x)) 2)

(funcall (lambda (x) (+ x 1)) 3)

(defun lambda-returner ()
  (lambda (x) (+ 1 x)))

(defun let-over-lambda-returner ()
  (let ((y 1))
    (lambda (x)
      (incf y x))))

(progn
  (compile 'let-over-lambda-returner)
  (time (let-over-lambda-returner)))

(defun counter ()
  (let ((counter 0))
    (lambda () (incf counter))))

(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list)))
        (dolist (c data)
          (if curr
              (setq curr
                    (if (char= (car curr) c)
                        (cdr curr)
                        trig))))
        (not curr)))))

(defmacro letfun (bindings &body body)
  (let (bindings-list (mapcar (lambda (x) (if (listp x) (car x) x)) bindings))
    (cond
      ((null bindings)
       ;; rewrite the body with any references
       `,@(mapcar (lambda (expr) (if (member (car expr) bindings-list)

                                     )) body)
       )
      ((listp (car bindings)) `((lambda (,(caar bindings)) (my-let ,(cdr bindings) ,@body)) ,@(cdar bindings)))
      (t `((lambda (,(car bindings)) (my-let ,(cdr bindings) ,@body)) nil)))))

(defmacro letfun (bindings &body body)
  "Allows one to lexically bind symbols defined in BINDINGS to functions then call them without the use of FUNCALL"
  (let ((bindings-list (mapcar (lambda (x) (if (listp x) (car x) x)) bindings)))
    `(let ,bindings
       ,@(mapcar
          (lambda (expr)
            (if (member (car expr) bindings-list)
                `(funcall ,@expr)
                expr))
          body)
       )))

(defmacro comment (&rest forms)
  (let ((stub-form (reduce (lambda (acc pair) (if (eq :stub (car pair)) (cadr pair) acc)) (zip forms (cdr forms)) :initial-value nil)))
    (when stub-form
      stub-form)
    ))

(defun zip (l1 l2)
  (mapcar (lambda (a b) (list a b)) l1 l2))

(comment
 "The output of"
 (letfun (myprint (scanner (block-scanner "hello world")))
   (setq myprint #'print)
   (myprint "hello world")
   (scanner "my god hxhello world how are you today?"))

 "is the following macro expansion"
 (LET (MYPRINT (SCANNER (BLOCK-SCANNER "hello world")))
   (SETQ MYPRINT #'PRINT)
   (FUNCALL MYPRINT "hello world")
   (FUNCALL SCANNER "my god hxhello world how are you today?")))

(assert (eq 5 (comment
 "you can also stub out a comment such that it'll return the evaluation of the form following the keyword :stub"
 (so I can have arbitrary evaluations in here)
 (but they won't break anything even if the symbols are unbound)

 even a stub can be broken as long as it's not the last one
 :stub (broken ass form)

 You cannot use commas though "," they break things

 "it will also only evaluate the last stub because of how it's programmed"
 :stub 5)))
