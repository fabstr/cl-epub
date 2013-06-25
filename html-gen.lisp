(in-package :html-gen)

;; The void elements according to http://www.w3.org/TR/html-markup/syntax.html#void-element
(defparameter *void-elements* '(:area :base :br :col :command :embed :hr :img 
				:input :keygen :link :meta :param :source :track :wbr))

(defun group (source n)
  "Paul Graham's group function."
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n  (gensym (concatenate 'string (string ',n) "-GENSYM-"))))
     ,@body))


(defun attrs-to-html (attrs)
  "With a list (foo \"bar\" monkey \"banana\"), return the string ' foo=\"bar\" monkey=\"banana\"'.
If attrs is null, return \"\"."
  (if (null attrs) ""
      (let ((grouped-attrs (group attrs 2)))
	(with-output-to-string (str)
	  (loop for a in grouped-attrs do (format str " ~(~a~)=\"~a\""  (car a) (cadr a)))))))

(defun html-statement-p (form)
  "Return t if form is a list and begins with a keyword and the second value is a list."
  (and (listp form)
       (keywordp (car form))
       (listp (cadr form))))

(defun void-element-p (tag)
  "Return t if the tag, being a keyword, is a void element."
  (if (find tag *void-elements*) t nil))

(defmacro create-attributes-string (attrs)
  (with-gensyms (grouped-attrs attr stream)
    (if (or (null attrs) (eql #\- attrs)) ""
	`(with-output-to-string (,stream)
	   (let ((,grouped-attrs (group ',attrs 2)))
	     (loop for ,attr in ,grouped-attrs do (format ,stream " ~a=\"~a\"" 
							  ;; if the first is a string, leave it as it is.
							  ;; else make it a downcase string
							  (if (stringp (car ,attr)) 
							      (car ,attr)
							      (string-downcase (string (car ,attr))))
							  (cadr ,attr))))))))

(defmacro html-2 (&rest statements)
  (if (not (null statements))
      (with-gensyms (str element attributes data)
	  `(with-output-to-string (,str)
	     ,@(loop 
		  for s in statements
		  if (html-statement-p s) collect `(let ((,element (string-downcase (first ',s)))
							 (,attributes (create-attributes-string ,(second s)))
							 (,data (html-2 ,@(cddr s))))
						     (if (void-element-p ,element)
							 (format ,str "<~a~a>" ,element ,attributes)
							 (format ,str "<~a~a>~a</~a>" ,element ,attributes ,data ,element)))
		  else collect `(format ,str "~a" ,s))))))

(let ((foo "you've been fooed!"))
  (labels ((monkey (arg) (format nil "The monkey says: ~s" arg)))
    (html-2
     "hey, " 
     (:b () (monkey foo))
     (:a (href "http://www.google.com") "This is a link to Google!")
     (:p () (:h1 () "header") "text"))))
