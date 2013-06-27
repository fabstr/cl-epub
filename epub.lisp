;; (in-package :epub)

(defparameter *book-name* nil)
(defparameter *book-title* nil)
(defparameter *sections* nil)
(defparameter *package-document-path* "Content/package-document.opf")
(defparameter *content-path* "Content/content.xhtml")

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

(defun write-container-xml ()
  (if (null *book-name*)
      (error "book-name is nil"))
  (let ((path "META-INF/container.xml"))
    (with-open-file-and-ensured-directories
	(stream path :direction :output :if-exists :supersede)
      (generate-xml
       (stream)
       (xml-declaration version "1.0" encoding "UTF-8")
       (:container
	(xmlns "urn:oasis:names:tc:opendocument:xmlns:container"
	       version "1.0")
	(:rootfiles
	 ()
	 (:rootfile
	  (full-path "Content/package-document.opf"
		     media-type "application/oebps-package+xml"))))))))

(defun write-content ()
  ;; the title and the sections are printed and must be bound to something
  (if (null *book-title*) (error "book-title is nil")
      (if (null *sections*) (error "sections is nil")))
  (with-open-file-and-ensured-directories (stream *content-path*
						  :direction :output
						  :if-exists :supersede)
    (generate-xml (stream)
		  (xml-declaration version "1.0" encoding "UTF-8")
		  (:html (xmlns "http://www.w3.org/1999/xhtml"
				"xml:lang" "en"
				"lang" "en"
				"xmlns:epub" "http://www.idpf.org/2007/ops")
			 (:head ()
				(:meta (charset "utf-8"))
				(:title () *book-title*)
				(:link (rel "stylesheet" type "text/css"
					    href "css.css")))
			 (:body () *sections*)))))

(defun write-package-document (metadata manifest spine
			       &optional guide bindings)
  "Create the package document and write it to path. metadata, manifest and
spine should be the elements (ie <metadata>...</metadata>) of the respective
tag. guide and bindings are by the EPUB standard optional. The package document
file name should end in .opf."
  (with-open-file-and-ensured-directories (stream *package-document-path*
						  :direction :output
						  :if-exists :supersede)
    (generate-xml (stream)
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
					  (:bindings () bindings)))))))

(defun make-keyword (name)
  (values (intern (string name) :keyword)))

(defmacro genif (form &optional add-dc)
  `(if ,form
       (xml (,(make-keyword (format nil "~:[~;dc:~]~a"
				    add-dc form)) () ,form))))

(defun create-metadata (identifier title language modified-timestamp
			&key meta link contributor coverage creator date
			  description format publisher relation rights source
			  object type)
  "Return a string of the <metadata>...</metadata> specified by the arguments to
this function. Please see
  http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-metadata-elem
for more information on the metadata element and its content."
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
