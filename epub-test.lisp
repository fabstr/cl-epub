(deftest t-write-mimetype ()
  (check
    nil))

(deftest t-with-open-file-and-ensured-directories ()
  (check
    nil))

(deftest t-write-string ()
  (check
    nil))

(deftest t-write-container-xml ()
  (check
    nil))

(deftest t-write-content ()
  (check
    nil))

(deftest t-write-package-document ()
  (check
    nil))

(deftest t-make-keyword ()
  (check
    nil))

(deftest t-genif ()
  (check
    nil))

;(deftest t-create-metadata ()
(let ((identifier "let-identifier")
     (title "let-title")
     (language "let-language")
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
  (create-metadata identifier title language :meta meta :link link
		   :contributor contributor :coverage coverage
		   :creator creator :date date :description description
		   :format format :publisher publisher :relation relation
		   :rights rights :source source :object object :type type))




(deftest t-epub ()
  (t-with-open-file-and-ensured-directories)
  (t-write-string)
  (t-write-container-xml)
  (t-write-content)
  (t-write-package-document)
  (t-make-keyword)
  (t-genif)
  (t-create-metadata))
