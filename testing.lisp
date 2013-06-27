;;; This testing framework was defined in Practical Common Lisp by Peter Seibel

(defparameter *test-name* nil)

(defun report-result (result form)
  (format *error-output* "~:[FAIL ~a: ~a~%~;~]" result *test-name* form)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym (format nil
							   "~a" (string ',n)))))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))
