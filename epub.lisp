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

;; (defun write-string (path string)
;;   "Write the string to the given file and ensure the directories exist."
;;   (with-open-file-and-ensured-directories (stream path :direction :output
;; 						  :if-exists :supersede)
;;     (format path string)))

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

(defun write-package-document (path metadata manifest spine
			       &optional guide bindings)
  "Create the package document and write it to path. metadata, manifest and
spine should be the elements (ie <metadata>...</metadata>) of the respective
tag. guide and bindings are by the EPUB standard optional."
  (with-open-file-and-ensured-directories (stream path :direction :output
						  :if-exists :supersede)
    (generate-html (stream)
		   "<?xml version=\"1.0\" encoding=\"UFT-8\"?>"
		   (:package (xmlns "http://www.idpf.org/2007/opf"
				    version "3.0"
				    unique-identifier "uid")
			     metadata
			     manifest
			     spine
			     (if guide (generate-html
					(stream)
					(:guide () guide)))
			     (if bindings (generate-html
					   (stream)
					   (:bindings bindings)))))))

(defun make-keyword (name)
  (values (intern (string name) :keyword)))

(defmacro genif (form &optional add-dc)
  `(if ,form
       (xml (,(make-keyword (format nil "~:[~;dc:~]~a" add-dc form)) () ,form)))))

(defun create-metadata (identifier title language modified-timestamp
			&key meta link contributor coverage creator date
			  description format publisher relation rights source
			  object type)
  (xml
   (:metadata ()
	      (:dc:identifier (id "uid") identifier)
	      (genif title t)
	      (genif language t)
	      (genif meta)
	      (:meta (property "dcterms:modified") modified-timestamp)
	      (genif link)
	      (genif contributor t)
	      (genif coverage t)
	      (genif creator t)
	      (genif date t)
	      (genif description t)
	      (genif format t)
	      (genif publisher t)
	      (genif relation t)
	      (genif rights t)
	      (genif source t)
	      (genif object t)
	      (genif type t))))
