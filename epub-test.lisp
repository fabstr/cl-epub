(defun test-cleanup ()
  (dolist (file '("mimetype" "META-INF/container.xml"))
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
  (let ((*book-title* "this is a book title")
	(*sections* (html (:section () "section1")
			  (:section () "section2")
			  (:section () "section3"))))
    (write-content)
    (with-open-file (stream *content-path*)
      (check
	(string= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" xmlns:epub=\"http://www.idpf.org/2007/ops\"><head><meta charset=\"utf-8\"></meta><title>this is a book title</title><link rel=\"stylesheet\" type=\"text/css\" href=\"css.css\"></link></head><body><section>section1</section><section>section2</section><section>section3</section></body></html>"
		 (read-line stream))))))

(deftest t-write-package-document ()
  (check
    nil))

(deftest t-make-keyword ()
  (check
    nil))

(deftest t-genif ()
  (check
    nil))

(deftest t-parameters ()
  (check
    (string= "Content/package-document.opf" *package-document-path*)
    (string= "Content/content.xhtml" *content-path*)))

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
    (create-metadata identifier title language modified
		     :meta meta :link link :contributor contributor
		     :coverage coverage :creator creator :date date
		     :description description :format format
		     :publisher publisher :relation relation :rights rights
		     :source source :object object :type type)))

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
