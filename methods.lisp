(defgeneric add-section (Epub Section)
  (:documentation "Add the section to the epub book. The section's index
specifies where in the reading order the section is to be put."))
(defmethod add-section ((e Epub) (s Section))
  ;; use the section's index as key in the hash table
  (setf (gethash (section-index s) (epub-sections e)) s))

(defgeneric add-paragraph (Section Paragraph)
  (:documentation "Add the paragraph to the section. The index of the paragraph
is used to place the paragraph at the correct position in the section, is a
paragraph with index 1 will be placed before index 2."))
(defmethod add-paragraph ((s Section) (p Paragraph))
  (setf (gethash (paragraph-index p) (section-paragraphs s)) p))

(defgeneric add-item-to-manifest (Epub Item)
  (:documentation "Add the Item to the epub's manifest."))
(defmethod add-item-to-manifest ((e Epub) (i Item))
  (setf (gethash (item-id i) (epub-manifest e)) i))

(defgeneric add-itemref-to-spine (Epub Itemref)
  (:documentation "Add the Itemref to the epub's spine. The itemrefs are added
in order, the one that should be read first is to be added first."))
(defmethod add-itemref-to-spine ((e Epub) (i Itemref))
  (push i (epub-spine e)))

(defgeneric serialize-to-html (Object)
  (:documentation "Serialize the object to html."))

(defmethod serialize-to-html ((i Item))
  (with-output-to-string (str)
    (format str "<item id=\"~a\" href=\"~a\" media-type=\"~a\""
	    (item-id i) (item-href i) (item-media-type i))
    (if (item-fallback i)
	(format str " fallback=\"~a\"" (item-fallback i)))
    (if (item-properties i)
	(format str " properties=\"~a\"" (item-properties i)))
    (if (item-media-overlay i)
	(format str " media-overlay=\"~a\"" (item-media-overlay i)))
    (format str "></item>")))

(defmethod serialize-to-html ((i Itemref))
  (with-output-to-string (str)
    (format str "<itemref idref=\"~a\"" (itemref-idref i))
    (if (itemref-linear i)
	(format str " linear=\"~a\"" (itemref-linear i)))
    (if (itemref-id i)
	(format str " id=\"~a\"" (itemref-id i)))
    (if (itemref-properties i)
	(format str " properties=\"~a\"" (itemref-properties i)))
    (format str "></itemref>")))

(defmethod serialize-to-html ((m Metadata))
  ;; the metadata must contain these four elements
  (if (null (metadata-identifier m))
      (error "The metadata content element identifier must be specified."))
  (if (null (metadata-title m))
      (error "The metadata content element title must be specified."))
  (if (null (metadata-language m))
      (error "The metadata content element language must be specified."))
  (if (null (metadata-modified-timestamp m))
      (error "The metadata content element meta with property modified must be
specified."))
  (with-output-to-string (str)
    (generate-xml
     (str)
     (:metadata ()
		;; the four obligatory elements
		(:dc:identifier () (metadata-identifier m))
		(:dc:title () (metadata-title m))
		(:dc:language () (metadata-language m))
		(:meta ((:property "dcterms:modified"))
		       (metadata-modified-timestamp m))

		;; the optional elements
		(if (metadata-meta m)
		    (format str (xml (:meta () (metadata-meta m)))))
		(if (metadata-link m)
		    (format str (xml (:link () (metadata-link m)))))
		(if (metadata-contributor m)
		    (format str (xml (:dc:contributor
				      () (metadata-contributor m)))))
		(if (metadata-coverage m)
		    (format str (xml (:dc:coverage () (metadata-coverage m)))))
		(if (metadata-creator m)
		    (format str (xml (:dc:creator () (metadata-creator m)))))
		(if (metadata-date m)
		    (format str (xml (:dc:date () (metadata-date m)))))
		(if (metadata-description m)
		    (format str
			    (xml (:dc:description
				  () (metadata-description m)))))
		(if (metadata-format m)
		    (format str (xml (:dc:format () (metadata-format m)))))
		(if (metadata-publisher m)
		    (format str (xml (:dc:publisher
				      () (metadata-publisher m)))))
		(if (metadata-relation m)
		    (format str (xml (:dc:relation () (metadata-relation m)))))
		(if (metadata-rights m)
		    (format str (xml (:dc:rights () (metadata-rights m)))))
		(if (metadata-source m)
		    (format str (xml (:dc:source () (metadata-source m)))))
		(if (metadata-subject m)
		    (format str (xml (:dc:subject () (metadata-subject m)))))
		(if (metadata-type m)
		    (format str (xml (:dc:type () (metadata-type m)))))))))

(defgeneric get-ordered-paragraphs (Section)
  (:documentation "Get the paragraphs, from the section, sorted according to the
index."))
(defmethod get-ordered-paragraphs ((s Section))
  (sort (loop for p being the hash-values in (section-paragraphs s) collect p)
	#'(lambda (p1 p2)
	    (< (paragraph-index p1)
	       (paragraph-index p2)))))

(defmethod serialize-to-html ((s Section))
  (with-output-to-string (stream)
    (generate-xml
     (stream)
     (:section ()
	       (loop
		  for p in (get-ordered-paragraphs s)
		  ;; the paragraphs already have html in the text
		  ;; field
		  do (format stream (paragraph-text p)))))))

(defgeneric manifest-to-html (Epub)
  (:documentation "Write the manifest to html and return a string."))
(defmethod manifest-to-html ((e Epub))
  (with-output-to-string (str)
    (generate-xml
     (str)
     (:manifest ()
		(loop
		   for item being the hash-values in (epub-manifest e)
		   do (format str (serialize-to-html item)))))))

(defgeneric spine-to-html (Epub)
  (:documentation "Write the spine to html and return a string."))
(defmethod spine-to-html ((e Epub))
  (with-output-to-string (str)
    (generate-xml
     (str)
     (:spine ((:toc "ncx"))
	     (loop
		for itemref in (reverse (epub-spine e))
		do (format str (serialize-to-html itemref)))))))

(defgeneric write-package-document (Epub path)
  (:documentation "Write the package document to the path."))
(defmethod write-package-document ((e Epub) (p String))
  (ensure-directories-exist p)
  (with-open-file (stream p :direction :output :if-exists :supersede)
    ;; (declare (ignore stream)) ; the stream is used in the generate-xml macro
    (generate-xml (stream)
		  (xml-declaration (:version "1.0") (:encoding "UTF-8"))
		  (:package ((:xmlns "http//www.idpf.org/2007/opf")
			     (:version "3.0")
			     (:unique-identifier "uid"))
			    (serialize-to-html (epub-metadata e))
			    (manifest-to-html e)
			    (spine-to-html e))))))

(defun write-container-xml (path)
  "Write the container.xml document to path (should be META-INF/container.xml.)"
  (ensure-directories-exist "META-INF/container.xml")
  (with-open-file (stream path :direction :output :if-exists :supersede)
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

(defun write-mimetype (path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream "application/epub+zip")))

(defgeneric get-ordered-sections (Epub)
  (:documentation "Get the sections sorted by the index."))
(defmethod get-ordered-sections ((e Epub))
  (sort (loop for s being the hash-values in (epub-sections e) collect s)
	#'(lambda (s1 s2) (< (section-index s1)
			     (section-index s2)))))

(defgeneric write-content (Epub p &optional css)
  (:documentation "Write the content document to p."))
(defmethod write-content ((e Epub) (p String) &optional css)
  (ensure-directories-exist p)
  (with-open-file (stream p :direction :output :if-exists :supersede)
    (generate-xml
     (stream)
     (xml-declaration (:version "1.0") (:encoding "UTF-8"))
     (:html ((:xmlns "http://www.w3.org/1999/xhtml")
	     (:xmlns:epub "http://www.idpf.org/2007/ops"))
	    (:head ()
		   (:meta ((:charset "utf-8")))
		   (:title () (metadata-title (epub-metadata e)))
		   (if css (format stream (xml (:link ((:rel "stylesheet")
						       (:href css)))))))
	    (:body ()
		   (loop
		      for section in (get-ordered-sections e)
		      do (format stream (serialize-to-html section))))))))

(defgeneric write-nav (Epub p &optional css)
  (:documentation "Write the navigation document to p"))
(defmethod write-nav ((e Epub) p &optional css)
  (ensure-directories-exist p)
  (with-open-file (stream p :direction :output :if-exists :supersede)
    (let ((sections (get-ordered-sections e)))
      (generate-xml
       (stream)
       (xml-declaration (:version "1.0") (:encoding "UTF-8"))
       (:html
	((:xmlns "http://www.w3.org/1999/xhtml")
	 (:xmlns:epub "http://www.idpf.org/2007/ops"))
	(:head ()
	       (:meta ((:charset "utf-8")))
	       (if css (format stream (xml (:link ((:rel "stylesheet")
						   (:href css)))))))
	(:body
	 ()
	 (:nav
	  ((:epub:type "toc") (:id "toc"))
	  (:ol
	   ()
	   (loop
	      for section in sections
	      if (section-add-to-toc-p section)
	      do (let ((title (section-title section)))
		   (if (string= "" title)
		       (error
			"A title of a section must not be the empty string.")
		       (format stream (xml (:li () title))))))))))))))
