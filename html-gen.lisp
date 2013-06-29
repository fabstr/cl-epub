;; (in-package :html-gen)

;; The void elements according to
;; http://www.w3.org/TR/html-markup/syntax.html#void-element
(defparameter *void-elements* '(:area :base :br :col
				:command :embed :hr :img
				:input :keygen :link :meta
				:param :source :track :wbr))

(defparameter *html-output* nil)

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
  "The with-gensyms macro, derived from Practical Common Lisp."
  `(let ,(loop
	    for n in names
	    collect `(,n  (gensym (format nil "~a-" (string ',n)))))
     ,@body))

(defun html-statement-p (form)
  "Return t if form is a list and begins with a keyword and the second value is
a list."
  (and (listp form)
       (keywordp (car form))
       (listp (cadr form))))

(defun void-element-p (tag)
  "Return t if the tag, being a string on the form \"html\", is a void element."
  (if (find  tag *void-elements*)
      t
      nil))

(defmacro create-attributes-string (attrs)
  "With a list (foo \"bar\" monkey \"banana\"), return the string
' foo=\"bar\" monkey=\"banana\"'. If attrs is null or #\-, return \"\"."
  (with-gensyms (grouped-attrs attr stream)
    (if (null attrs) ""
	(if (not (listp attrs))
	    (error "Not a list: ~s" attrs)
	    `(with-output-to-string (,stream)
	       (let ((,grouped-attrs (group ',attrs 2)))
		 (loop
		    for ,attr in ,grouped-attrs
		    do (format ,stream " ~a=\"~a\""
			       ;; if the first is a string, leave it as it is.
			       ;; else make it a downcase string
			       (if (stringp (car ,attr))
				   (car ,attr)
				   (string-downcase (string (car ,attr))))
			       (cadr ,attr)))))))))

(defmacro generate-html ((stream &key (close-void t)) &rest statements)
  "Write html output to the stream. statements should be html statements or
something that can be passed to format's \"~a\". If close-void is t, end void
elements with a '\' (ie <meta ... />). Please see html for how the statements
should look."
  (if (not (null statements))
      (with-gensyms (element attributes element-string)
	`(progn
	   ,@(loop
		for s in statements
		if (not (null s))
		if (html-statement-p s)
		collect `(let* ((,element (first ',s))
			       (,element-string (string-downcase ,element))
			       (,attributes (create-attributes-string
					     ,(second s))))
			   (format ,stream "<~a~a~:[~; /~]>"
				   ,element-string
				   ,attributes
				   (and ,close-void (void-element-p ,element)))
			   (if (not (void-element-p ,element))
			       (progn
				 (generate-html (,stream
						 :close-void ,close-void)
						,@(cddr s))
				 (format ,stream "</~a>" ,element-string))))
		else collect `(if ,s (format ,stream "~a" ,s) (format ,stream "")))))))

(defmacro html (&rest statements)
  "Generate a string of the html.
Each statement is written on the form (element list-of-attributes data).
Examples:
  (:p () \"paragraph\") => \"<p>paragraph</p>\"
  (:a (href \"http://www.example.com\") \"Link to www.example.com\") =>
      \"<a href=\"\"http://www.example.com\"\">Link to www.example.com</a>\"

The tags can be nested:
  (:div (id \"box\")
        (:h1 () \"header\")
        (:p () \"Lorem lipsum ...\"))
which is transformed to
\"<div id=\"\"box\"\"><h1>header</h1><p>Lorem lipsum ...</p></div>\"

If the element is a void element, the data is not written.

The html generation takes place in the current lexical environment, thus
  (let ((foo \"bar\"))
    (labels ((my-func (arg) (format nil \"you said ~s\" arg)))
      (html (:h1 () foo)
  	    (:p () (my-func \"hello\")))))
will return \"<h1>bar</h1><p>you said \"\"hello\"\"</p>\"."
  (with-gensyms (stream)
    `(with-output-to-string (,stream)
       (generate-html (,stream) ,@statements))))

(defmacro xml (&rest statements)
  "Like html, except for the void elements have closing tags and content:
<meta ...>...</meta> instead of <meta ... />"
  ;; by binding void-elements to nil, no element can be found  in
  ;; *void-elements* and void-element-p will always return nil ==> there are no
  ;; void elements ==> all elements prints closing tags and content
  `(let ((*void-elements* nil))
     (html ,@statements)))

(defmacro generate-xml ((&rest args) &rest statements)
  "A mixture of generate-html and xml (ie *void-elements* are bound to nil)."
  `(let ((*void-elements* nil))
     (generate-html (,@args) ,@statements)))

(defmacro xml-declaration (&rest attributes)
  `(format nil "<?xml~a?>" (create-attributes-string ,attributes)))
