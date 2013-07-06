(deftest t-section ()
  (let ((s (make-instance 'Section
			  :index 0
			  :add-to-toc-p t
			  :title "title")))
    (check
      (= 0 (section-index s))
      (eq t (section-add-to-toc-p s))
      (string= "title" (section-title s))
      (eq t (hash-table-p (section-paragraphs s))))))

(deftest t-paragraph ()
  (let ((p (make-instance 'Paragraph
			  :index 0
			  :text "<p>foo</p>")))
    (check
      (= 0 (paragraph-index p))
      (string= "<p>foo</p>" (paragraph-text p)))))

(deftest t-item ()
  (let ((i (make-instance 'Item
			  :id "id"
			  :href "href"
			  :media-type "media-type"
			  :fallback "fallback"
			  :properties "properties"
			  :media-overlay "media-overlay")))
    (check
      (string= "id" (item-id i))
      (string= "href" (item-href i))
      (string= "media-type" (item-media-type i))
      (string= "fallback" (item-fallback i))
      (string= "properties" (item-properties i))
      (string= "media-overlay" (item-media-overlay i)))))


(deftest t-itemref ()
  (let ((i (make-instance 'Itemref
			  :idref "idref"
			  :linear "linear"
			  :id "id"
			  :properties "properties")))
    (check
      (string= "idref" (itemref-idref i))
      (string= "linear" (itemref-linear i))
      (string= "id" (itemref-id i))
      (string= "properties" (itemref-properties i)))))

(deftest t-metadata ()
  (let ((m (make-instance 'Metadata
			  :identifier "identifier"
			  :title "title"
			  :language "language"
			  :modified-timestamp "modified-timestamp"
			  :meta "meta"
			  :link "link"
			  :contributor "contributor"
			  :coverage "coverage"
			  :date "date"
			  :description "description"
			  :format "format"
			  :publisher "publisher"
			  :relation "relation"
			  :rights "rights"
			  :source "source"
			  :subject "subject"
			  :type "type")))
    (check
      (string= "identifier" (metadata-identifier m))
      (string= "title" (metadata-title m))
      (string= "language" (metadata-language m))
      (string= "modified-timestamp" (metadata-modified-timestamp m))
      (string= "meta" (metadata-meta m))
      (string= "link" (metadata-link m))
      (string= "contributor" (metadata-contributor m))
      (string= "coverage" (metadata-coverage m))
      (string= "date" (metadata-date m))
      (string= "description" (metadata-description m))
      (string= "format" (metadata-format m))
      (string= "publisher" (metadata-publisher m))
      (string= "relation" (metadata-relation m))
      (string= "rights" (metadata-rights m))
      (string= "source" (metadata-source m))
      (string= "subject" (metadata-subject m))
      (string= "type" (metadata-type m)))))

(deftest t-epub ()
  (let ((e (make-instance 'Epub
			  :sections '("these are" "the sections"))))
    (check
      (equal '("these are" "the sections") (epub-sections e))
      (hash-table-p (epub-toc e))
      (hash-table-p (epub-manifest e))
      (eq 'Metadata (type-of (epub-metadata e)))
      (eq nil (epub-spine e)))))

(deftest test-classes ()
    (check
     (t-section)
     (t-paragraph)
     (t-item)
     (t-itemref)
     (t-metadata)
     (t-epub)))
