(defun test-cleanup ()
  (dolist (file (list *container-xml-path* *package-document-path*
		      *content-path* *nav-path*))
    (delete-file file)))

(deftest t-write-mimetype ()
  (write-mimetype)
  (with-open-file (stream "mimetype")
    (check
      (string= "application/epub+zip" (read-line stream)))))

(deftest t-with-open-file-and-ensured-directories ()
  (check
    nil))

(deftest t-write-container-xml ()
  (write-container-xml)
  (with-open-file (stream "META-INF/container.xml")
    (check
      (string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><container xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\" version=\"1.0\"><rootfiles><rootfile full-path=\"Content/package-document.opf\" media-type=\"application/oebps-package+xml\"></rootfile></rootfiles></container>"
	       (read-line stream)))))

(deftest t-write-content ()
  (let* ((*book-title* "this is a book title")
	 (p1 (make-instance 'Paragraph :id 1 :text (html (:p () "p1"))))
	 (p2 (make-instance 'Paragraph :id 2 :text (html (:p () "p2"))))
	 (p3 (make-instance 'Paragraph :id 3 :text (html (:p () "p3"))))
	 (p4 (make-instance 'Paragraph :id 4 :text (html (:p () "p4"))))
	 (*sections* (list (make-instance 'Section
					  :id 0
					  :paragraphs (list p1 p2))
			   (make-instance 'Section
					  :id 1
					  :paragraphs (list p3 p4)))))
    (write-content)
    (with-open-file (stream *content-path*)
      (check
	(string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" xmlns:epub=\"http://www.idpf.org/2007/ops\"><head><meta charset=\"utf-8\"></meta><title>this is a book title</title><link rel=\"stylesheet\" type=\"text/css\" href=\"css.css\"></link></head><body><section id=\"s0\"><p>p1</p><p>p2</p></section><section id=\"s1\"><p>p3</p><p>p4</p></section></body></html>"
		 (read-line stream))))))

(deftest t-write-package-document ()
  (let ((metadata (xml (:metadata () "this is metadata")))
	(manifest (xml (:manifest () "this is manifest")))
	(spine (xml (:spine () "this is spine")))
	(guide (xml (:guide () "this is a guide")))
	(bindings (xml (:bindings () "these are bindings"))))
    (check
     (progn
       (write-package-document metadata manifest spine)
       (with-open-file (stream *package-document-path*)
	 (string= "<?xml version=\"1.0\" encoding=\"UFT-8\"?><package xmlns=\"http://www.idpf.org/2007/opf\" version=\"3.0\" unique-identifier=\"uid\"><metadata>this is metadata</metadata><manifest>this is manifest</manifest><spine>this is spine</spine></package>"
		  (read-line stream))))
     (progn
       (write-package-document metadata manifest spine guide bindings)
       (with-open-file (stream *package-document-path*)
     	 (string= "<?xml version=\"1.0\" encoding=\"UFT-8\"?><package xmlns=\"http://www.idpf.org/2007/opf\" version=\"3.0\" unique-identifier=\"uid\"><metadata>this is metadata</metadata><manifest>this is manifest</manifest><spine>this is spine</spine><guide>this is a guide</guide><bindings>these are bindings</bindings></package>"
		  (read-line stream)))))))


(deftest t-make-keyword ()
  (check
    (eql :foo (make-keyword 'foo))
    (eql :|foo| (make-keyword "foo"))))

(deftest t-parameters ()
  (check
    (string= "Content/package-document.opf" *package-document-path*)
    (string= "Content/content.xhtml" *content-path*)
    (string= "nav" *nav-id*)
    (string= "Content/nav.xhtml" *nav-path*)))

(deftest t-create-metadata ()
  (let ((identifier "let-identifier")
	(title "let-title")
	(language "let-language")
	(modified "today")
	(meta "let-meta")
	(link "let-link")
	(contributor "let-contributor")
	(coverage "let-coverage")
	(creator "let-creator")
	(date "let-date")
	(description "let-description")
	(format "let-format")
	(publisher "let-publisher")
	(relation "let-relation")
	(rights "let-rights")
	(source "let-source")
	(object "let-object")
	(type "let-type"))
    (check
      ;; try all the arguments
      (string= "<metadata><dc:identifier id=\"uid\">let-identifier</dc:identifier><meta property=\"dcterms:modified\">today</meta><dc:title>let-title</dc:title><dc:language>let-language</dc:language><meta>let-meta</meta><link>let-link</link><dc:contributor>let-contributor</dc:contributor><dc:coverage>let-coverage</dc:coverage><dc:creator>let-creator</dc:creator><dc:date>let-date</dc:date><dc:description>let-description</dc:description><dc:format>let-format</dc:format><dc:publisher>let-publisher</dc:publisher><dc:relation>let-relation</dc:relation><dc:rights>let-rights</dc:rights><dc:source>let-source</dc:source><dc:object>let-object</dc:object><dc:type>let-type</dc:type></metadata>"
   	       (create-metadata identifier title language modified
				:meta meta :link link :contributor contributor
				:coverage coverage :creator creator :date date
				:description description :format format
				:publisher publisher :relation relation
				:rights rights :source source :object object
				:type type))
      ;; try some arguments
      (string= "<metadata><dc:identifier id=\"uid\">let-identifier</dc:identifier><meta property=\"dcterms:modified\">today</meta><dc:title>let-title</dc:title><dc:language>let-language</dc:language><meta>let-meta</meta><dc:contributor>let-contributor</dc:contributor></metadata>"
	       (create-metadata identifier title language modified
				:meta meta :contributor contributor)))))

(deftest t-create-item ()
  (check
    ;; check with the required args
    (string= "<item id=\"id\" href=\"my href\" media-type=\"media type\"></item>"
	     (create-item "id" "my href" "media type"))

    ;; check with an optional arg
    (string= "<item id=\"id\" href=\"my href\" media-type=\"media type\" properties=\"props\"></item>"
	     (create-item "id" "my href" "media type" :properties "props"))

    ;; test with all args
    (string= "<item id=\"id\" href=\"my href\" media-type=\"media type\" fallback=\"fall\" properties=\"props\" media-overlay=\"media\"></item>"
	     (create-item "id" "my href" "media type"
			  :fallback "fall" :properties "props"
			  :media-overlay "media"))))

(deftest t-create-manifest ()
  (check
    (string= "<manifest><item id=\"nav\" href=\"nav.xhtml\" media-type=\"application/xhtml+xml\" properties=\"nav\"></item></manifest>"
	     (create-manifest))
    (string= "<manifest><item id=\"nav\" href=\"nav.xhtml\" media-type=\"application/xhtml+xml\" properties=\"nav\"></item>i1i2</manifest>"
	     (create-manifest "i1" "i2"))))

(deftest t-create-spine ()
  (check
    (string= "<spine></spine>" (create-spine))
    (string= "<spine>i1i2</spine>" (create-spine "i1" "i2"))))

(deftest t-create-itemref ()
  (check
    (string= "<itemref idref=\"ref\"></itemref>" (create-itemref "ref"))
    (string= "<itemref idref=\"ref\" linear=\"no\"></itemref>"
	     (create-itemref "ref" :linear "no"))
    (string= "<itemref idref=\"ref\" linear=\"no\" id=\"id\" properties=\"props\"></itemref>"
	     (create-itemref "ref" :linear "no" :id "id" :properties "props"))))

(deftest t-section ()
  (let ((s1 (make-instance 'Section :id "id"
			   :paragraphs '("these are" "my paragraphs")))
	(s2 (make-instance 'Section :id "id2" :add-to-toc t
			   :paragraphs '("my paragraphs" "are short"))))
    (check
      ;; test the readers of the Section class
      (string= "id" (section-id s1))
      (eql nil (section-add-to-toc s1))
      (eql t (section-add-to-toc s2))
      (equal '("these are" "my paragraphs") (section-paragraphs s1)))))

(deftest t-defsection ()
  ;; test the defsection macro works
  (let ((*sections* nil))
    (defsection ("my id" :add-to-toc t) "these" "are" "my" "paragraphs")
    (check
      ;; test the length of *sections* is 1
      (eql nil (cdr *sections*))

      ;; test the fields of the section
      (string= "my id" (section-id (car *sections*)))
      (eql t (section-add-to-toc (car *sections*)))
      (equal '("these" "are" "my" "paragraphs")
	     (section-text (car *sections*))))))

(deftest t-section-to-html ()
  (let ((*sections* nil))
    (let ((sec (make-instance 'Section :id "my id" :add-to-toc t
			      :text '("this is my sentence "
				      "and some more words"))))
      (check
	(string= "<section id=\"my id\">this is my sentence and some more words</section>"
		 (section-to-html sec))))))

(deftest t-add-paragraph-to-section ()
  (let ((s (make-instance 'Section :id "id" :paragraphs nil))
	(p1 (make-instance 'Paragraph :id 0 :text "one"))
	(p2 (make-instance 'Paragraph :id 1 :text "two"))
	(p3 (make-instance 'Paragraph :id 3 :text "three")))
    (add-paragraph-to-section s p3)
    (add-paragraph-to-section s p2)
    (add-paragraph-to-section s p1)
    (check
      (string= "one" (paragraph-text (first (section-paragraphs s))))
      (string= "two" (paragraph-text (second (section-paragraphs s))))
      (string= "three" (paragraph-text (third (section-paragraphs s)))))))

(deftest t-get-sorted-paragraphs ()
  (let ((s (make-instance 'Section :id "id" :paragraphs nil)))
    (add-paragraph-to-section s (make-instance 'Paragraph :id 2 :text "2"))
    (add-paragraph-to-section s (make-instance 'Paragraph :id 0 :text "0"))
    (add-paragraph-to-section s (make-instance 'Paragraph :id 1 :text "1"))
    (add-paragraph-to-section s (make-instance 'Paragraph :id 3 :text "3"))
    (let ((sorted (mapcar #'(lambda (p)	(paragraph-text p))
			  (get-sorted-paragraphs s))))
      (check
	(string= "0" (first sorted))
	(string= "1" (second sorted))
	(string= "2" (third sorted))
	(string= "3" (fourth sorted))))))

(deftest t-epub ()
  (t-with-open-file-and-ensured-directories)
  (t-write-string)
  (t-write-container-xml)
  (t-write-content)
  (t-write-package-document)
  (t-make-keyword)
  (t-genif)
  (t-package-document-path)
  (t-create-metadata))
