(defparameter *book-name* "book_name")
(defparameter *book-title* "book-title")
(defparameter *sections* (html '(:section () "section 1")
			       '(:section () "section 2")))

(defun write-mimetype ()
  (with-open-file (stream "mimetype" :direction :output :if-exists :supersede)
    (format stream "application/epub+zip")))

(defmacro with-open-file-and-ensured-directories
    ((stream path &rest args) &body body)
  "Ensure the directories to path exists before calling with-open-file.
stream, path, args and body works as with with-open-file."
  `(progn
     (ensure-directories-exist ,path)
     (with-open-file (,stream ,path ,@(if args args))
       ,@body)))

(defun write-string (path string)
  "Write the string to the given file and ensure the directories exist."
  (with-open-file-and-ensured-directories (stream path :direction :output
						  :if-exists :supersede)
    (format path string)))

(defun write-container-xml (book-name)
  (write-string "META-INF/container.xml"
		(html
		 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
		 (:container
		  (xmlns "urn:oasis:names:tc:opendocument:xmlns:container"
			 version "1.0")
		  (:rootfiles
		   ()
		   (:roofile
		    (full-path
		     (format nil "EPUB/~a.opf" book-name)
		     media-type "application/oebps-package+xml")))))))

(defun write-content (book-name book-title sections)
  (write-string (format nil "Content/~a.xhtml" book-name)
		(html "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
		      (:html (xmlns "http://www.w3.org/1999/xhtml"
				    "xml:lang" "en"
				    "lang" "en"
				    "xmlns:epub" "http://www.idpf.org/2007/ops")
			     (:head ()
				    (:meta (charset "utf-8"))
				    (:link (rel "stylesheet"
						type "text/css"
						href (format nil "~a.css"
							     book-name)))
				    (:title () book-title)
				    (:body () sections))))))
