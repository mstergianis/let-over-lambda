(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defun fact (x)
  (when (< x 0) (error "cannot take factorial of a negative number"))
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

(defun choose (n r)
  (when (or (> r n) (< r 0)) (error "r must be in the range 0..n"))
  (if (or (= r 0) (= r n))
      1
      (/ (fact n)
         (fact (- n r))
         (fact r))))

(defmacro my-let (bindings &body body)
  (cond
    ((null bindings) `((lambda () ,@body)))
    ((listp (car bindings)) `((lambda (,(caar bindings)) (my-let ,(cdr bindings) ,@body)) ,@(cdar bindings)))
    (t `((lambda (,(car bindings)) (my-let ,(cdr bindings) ,@body)) nil))))
(assert (eq (my-let (x (y 6)) (setf x 5) (+ x y)) 11))

(defvar temp-special)

(setf temp-special 4)

(defun temp-special-returner ()
  temp-special)

(assert (eq 6 (my-let ((temp-special 6)) (temp-special-returner))))


(defun register-allocated-fixnum ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((acc 0))
    (loop for i from 1 to 100 do
      (incf (the fixnum acc)
            (the fixnum i)))
    acc))
