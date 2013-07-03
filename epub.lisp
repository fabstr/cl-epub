;; (in-package :epub)

(defparameter *book-name* nil)
(defparameter *book-title* nil)
(defparameter *sections* nil)
(defparameter *container-xml-path* "META-INF/container.xml")
(defparameter *package-document-path* "Content/package-document.opf")
(defparameter *content-path* "Content/content.xhtml")
(defparameter *nav-id* "nav")
(defparameter *nav-path* "Content/nav.xhtml")

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
    (with-open-file-and-ensured-directories
	(stream *container-xml-path* :direction :output :if-exists :supersede)
      (generate-xml
       (stream)
       (xml-declaration (:version "1.0") (:encoding "UTF-8"))
       (:container
	((:xmlns "urn:oasis:names:tc:opendocument:xmlns:container")
	 (:version "1.0"))
	(:rootfiles
	 ()
	 (:rootfile
	  ((:full-path "Content/package-document.opf")
	   (:media-type "application/oebps-package+xml"))))))))

(defun write-content ()
  ;; the title and the sections are printed and must be bound to something
  (if (null *book-title*) (error "book-title is nil")
      (if (null *sections*) (error "sections is nil")))
  (with-open-file-and-ensured-directories (stream *content-path*
						  :direction :output
						  :if-exists :supersede)
    (generate-xml
     (stream)
     (xml-declaration (:version "1.0") (:encoding "UTF-8"))
     (:html
      ((:xmlns "http://www.w3.org/1999/xhtml")
       (:xml:lang "en")
       (:lang "en")
       (:xmlns:epub "http://www.idpf.org/2007/ops"))
      (:head ()
	     (:meta ((:charset "utf-8")))
	     (:title () *book-title*)
	     (:link ((:rel "stylesheet")
		     (:type "text/css")
		     (:href "css.css"))))
      (:body ()
	     (dolist (s (sort *sections*
			      #'(lambda (s1 s2)
				  (let ((i1 (section-id s1))
					(i2 (section-id s2)))
				    (if (or (not (integerp i1))
					    (not (integerp i2)))
					(error
					 "~s and ~s should have integer id's"
					 s1 s2))
				    (< i1 i2)))))
	       (format stream (section-to-html s))))))))

(defun write-package-document (metadata manifest spine
			       &optional (guide "")  (bindings ""))
  "Create the package document and write it to path. metadata, manifest and
spine should be the elements (ie metadata is the string
  \"<metadata>...</metadata>\") of the respective tag. guide and bindings are by
the EPUB standard optional. The package document file name should end in .opf."
  (with-open-file-and-ensured-directories (stream *package-document-path*
						  :direction :output
						  :if-exists :supersede)
    (generate-xml (stream)
		  (xml-declaration (:version "1.0") (:encoding "UTF-8"))
		  (:package ((:xmlns "http://www.idpf.org/2007/opf")
			     (:version "3.0")
			     (:unique-identifier "uid"))
			    metadata
			    manifest
			    spine
			    (if guide guide)
			    (if bindings bindings)))))

(defun make-keyword (name)
  (values (intern (string name) :keyword)))

(defmacro metadata-expander (element stream &optional add-dc)
  `(if ,element (format ,stream (xml (,(make-keyword
				     (format nil "~:[~;dc:~]~a" add-dc element))
				       () ,element)))))

(defmacro metadata-expander-collector (&rest forms)
  `(progn
     ,@(loop for f in forms collect `(metadata-expander ,@f))))

(defun create-metadata (identifier title language modified-timestamp
			&key meta link contributor coverage creator date
			  description format publisher relation rights source
			  object type)
  "Return a string of the <metadata>...</metadata> specified by the arguments to
this function. Please see
  http://www.idpf.org/epub/30/Spec/epub30-publications.html#sec-metadata-elem
for more information on the metadata element and its content."
  (with-output-to-string (stream)
    (generate-xml
     (stream)
     (:metadata
      ()
      ;; these are required
      (generate-xml (stream)
		    (:dc:identifier ((:id "uid")) identifier)
		    (:meta ((:property "dcterms:modified")) modified-timestamp))
      (metadata-expander-collector
       ;; these two are also required
       (title stream t)
       (language stream t)

       ;; these are optional
       (meta stream)
       (link stream)
       (contributor stream t)
       (coverage stream t)
       (creator stream t)
       (date stream t)
       (description stream t)
       (format stream t)
       (publisher stream t)
       (relation stream t)
       (rights stream t)
       (source stream t)
       (object stream t)
       (type stream t))))))

(defun create-item (id href media-type &key fallback properties media-overlay)
  "Please see (somewhere) for information on the <item> element."
  (with-output-to-string (str)
    (format str "<item id=\"~a\" href=\"~a\" media-type=\"~a\""
	    id href media-type)
    (if fallback
	(format str " fallback=\"~a\"" fallback))
    (if properties
	(format str " properties=\"~a\"" properties))
    (if media-overlay
	(format str " media-overlay=\"~a\"" media-overlay))
    (format str "></item>")))

(defun create-manifest (&rest items)
  "With items being the item elements,ie
    (create-manifest \"<item ...></item> <item...></item>\"),
return the manifest."
  (with-output-to-string (stream)
    (generate-xml (stream)
		  (:manifest ()
			     (create-item "nav" "nav.xhtml"
					  "application/xhtml+xml"
					  :properties "nav")
			     (loop for i in items do (format stream i))))))

(defun create-spine (&rest itemrefs)
  "Create the spine element with the item references."
  (with-output-to-string (stream)
    (generate-xml (stream)
		  (:spine ()
			  (loop for i in itemrefs do (format stream i))))))

(defun create-itemrref (idref &key linear id properties)
  "Create an <itemref> tag. idref should, via idref, point to an <item> tag in
the manifest."
  (with-output-to-string (str)
    (format str "<itemref idref=\"~a\"" idref)
    (if linear
	(format str " linear=\"~a\"" linear))
    (if id
	(format str " id=\"~a\"" id))
    (if properties
	(format str " properties=\"~a\"" properties))
    (format str "></itemref>")))

(defclass Section ()
  ((id
    :initarg :id
    :initform (error "Must supply an id for the section.")
    :documentation "The id of the <section> element."
    :reader section-id)
   (add-to-toc
    :initarg :add-to-toc
    :initform nil
    :documentation "T if the section is to be added to the table of contents.
Else nil."
    :reader section-add-to-toc)
   (paragraphs
    :initarg :paragraphs
    :initform (error "Must supply the paragraphs for the section.")
    :documentation "The paragraphs of the section."
    :accessor section-paragraphs)))

(defmacro defsection ((id &key add-to-toc) &rest paragraphs)
  (with-gensyms (sec)
    `(let ((,sec (make-instance 'Section
				:id ,id
				:add-to-toc ,add-to-toc
				:paragraphs ',paragraphs)))
       (push ,sec *sections*)))))

(defgeneric section-to-html (Section))
(defmethod section-to-html ((s Section))
  "Return a <section>...</section> tag of the section."
  (with-output-to-string (str)
    (generate-xml (str)
		  (:section ((:id (format nil "s~a" (section-id s))))
			    (loop
			       for p in (section-paragraphs s)
			       do (format str "~a" (paragraph-text p)))))))

(defclass Paragraph ()
  ((id
    :initarg :id
    :initform (error "Must have an id for the paragraph.")
    :documentation "The id of the paragraph. When a Section is rendered to html,
the paragraphs are ordered according to the id's."
    :reader paragraph-id)
   (text
    :initarg :text
    :initform (error "The paragraph must have text.")
    :documentation "The text of the paragraph."
    :reader paragraph-text)))

(defgeneric add-paragraph-to-section (Section Paragraph))
(defmethod add-paragraph-to-section ((s Section) (p Paragraph))
  "Add the paragraph to the section by push-ing it to the list of paragraphs in
the section."
  (push p (section-paragraphs s)))

(defgeneric get-sorted-paragraphs (Section))
(defmethod get-sorted-paragraphs ((s Section))
  "Return a sorted list (ascending) of the paragraphs in the section."
  (let ((paragraphs (section-paragraphs s)))
    (sort paragraphs #'(lambda (p1 p2)
			 (< (paragraph-id p1) (paragraph-id p2))))))

(defun write-files ()
  (write-mimetype)
  (write-container-xml)
  (write-content)
  (write-package-document 'm 'm 's))
