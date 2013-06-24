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

(defun attrs-to-html (attrs)
  "With a list (foo \"bar\" monkey \"banana\"), return the string ' foo=\"bar\" monkey=\"banana\"'.
If attrs is null, return \"\"."
  (if (null attrs) ""
      (let ((grouped-attrs (group attrs 2)))
	(with-output-to-string (str)
	  (loop for a in grouped-attrs do (format str " ~a=\"~a\"" (string-downcase (string (car a))) (cadr a)))))))

(defun html-statement-p (form)
  "Return t if form is a list and begins with a keyword and the second value is a list."
  (and (listp form)
       (keywordp (car form))
       (listp (cadr form))))

(defun void-element-p (tag)
  "Return t if the tag, being a keyword, is a void element."
  (if (find tag *void-elements*) t nil))

(defun html (&rest statements)
  (with-output-to-string (str)
    (labels ((generate-html-tag (tag attrs &rest values)
	       ;; first get the tag as a lowercase string and the attributes written correctly
	       (let ((tag-string (string-downcase (string tag)))
		     (attrs-html (attrs-to-html attrs)))
		 ;; make difference in printing void elements and non-void elements
		 (if (void-element-p tag)
		     ;; is is a void-element, only print the tag and the attributes
		     (format str "<~a~a>" tag-string attrs-html)
		     (progn ;; it is a non-void element, print also the values and the end tag
		       (format str "<~a~a>" tag-string attrs-html)
		       (loop for val in values do (the-iterator val))
		       (format str "</~a>" tag-string)))))
	     (the-iterator (list-of-statements)
	       ;; for each statement, if it is a html statement, call to-html. 
	       ;; else, print the statement as it is
	       (loop 
		  for stmt in list-of-statements 
		  do (if (html-statement-p stmt)
			 (generate-html-tag (first stmt) (second stmt) (cddr stmt))
			 (format str "~a" (eval stmt))))))
      (the-iterator statements))))
