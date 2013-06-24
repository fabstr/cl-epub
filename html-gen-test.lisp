(deftest t-html-statement-p ()
  (check 
   ;; this should work
   (equal t (html-statement-p '(:p () "foo")))

   ;; check something that is not a list
   (equal nil (html-statement-p '"foo"))

   ;; check with args not a list
   (equal nil (html-statement-p '(:p "foo" "bar")))
   
   ;; check with not a keyword
   (equal nil (html-statement-p '(format nil "foo")))))

(deftest t-attrs-to-html ()
  (check 
    (string= "" (attrs-to-html ()))
    (string= " foo=\"bar\"" (attrs-to-html '(foo "bar")))))

(deftest t-void-element-p ()
  (check
    ;; check all the void elements are found
    (equal t (void-element-p :area))
    (equal t (void-element-p :base))
    (equal t (void-element-p :br))
    (equal t (void-element-p :col))
    (equal t (void-element-p :command))
    (equal t (void-element-p :embed))
    (equal t (void-element-p :hr))
    (equal t (void-element-p :img))
    (equal t (void-element-p :input))
    (equal t (void-element-p :keygen))
    (equal t (void-element-p :link))
    (equal t (void-element-p :meta))
    (equal t (void-element-p :param))
    (equal t (void-element-p :source))
    (equal t (void-element-p :track))
    (equal t (void-element-p :wbr))))

(deftest t-html ()
  (check 
    ;; test simple strings
    (string= "this is a string" (html "this is a string"))
    (string= "many strings" (html "many"     " "     "stri"    "ngs"))

    ;; test a simple case with no args
    (string= "<p>a paragraph</p>" (html '(:p () "a paragraph")))

    ;; test a simple case with args
    (string= "<a href=\"http://www.google.com\">google</a>" 
    	     (html '(:a (href "http://www.google.com") "google")))

    ;; test nested tags
    (string= "<p>a paragraph with a link to <a href=\"http://www.google.com\">google</a></p>"
    	     (html '(:p () "a paragraph with a link to " (:a (href "http://www.google.com") "google"))))
    
    ;; test parallell tags
    (string= "<h1>header</h1><p>paragraph</p>" (html '(:h1 () "header")
						      ' (:p () "paragraph")))
    
    ;; test a void element
    (string= "<input type=\"text\" value=\"write something\">" (html '(:input (type "text" value "write something"))))

    ;; test no values is printed with void elements
    (string= "<br>" (html '(:br () "this is not printed")))))

(defparameter *var* "variable")
(defun func (arg) (string arg))

(deftest t-html-variable-and-functions ()
  (check 
    (string= "<p>variable</p>" (html '(:p () *var*)))
    (string= "<p>function</p>" (html '(:p () (func "function"))))))

(deftest do-tests ()
  (check
    (t-html-statement-p)
    (t-attrs-to-html)
    (t-void-element-p)
    (t-html-variable-and-functions)
    (t-html)))
