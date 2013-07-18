;; First we need some paragraphs:
(defparameter *par1* (make-instance 'Paragraph
				    :index 0
				    :text "<p>This is the first paragrah</p>"))
(defparameter *par2* (make-instance 'Paragraph
				    :index 1
				    :text "<p>This is the second paragrah</p>"))
(defparameter *par3* (make-instance 'Paragraph
				    :index 0
				    :text "<p>This is the third paragrah</p>"))
(defparameter *par4* (make-instance 'Paragraph
				    :index 1
				    :text "<p>This is the fourth paragrah</p>"))

;; Then some sections (which are chapters)
(defparameter *chap1*
  (make-instance 'Section
		 :index 0
		 :add-to-toc-p t
		 :title "<a href=\"Content.xhtml#chap1\">Chapter 1</a>"))
(defparameter *chap2*
  (make-instance 'Section
		 :index 1
		 :add-to-toc-p t
		 :title "<a href=\"Content.xhtml#chap2\">Chapter 2</a>"))

;; Add the paragraphs to the sections:
(add-paragraph *chap1* *par1*)
(add-paragraph *chap1* *par2*)

(add-paragraph *chap2* *par3*)
(add-paragraph *chap2* *par4*)

;; Create the book object:
(defparameter *book* (make-instance 'Epub))

;; Add the sections
(add-section *book* *chap1*)
(add-section *book* *chap2*)

;; Add the metadata
(let ((md (epub-metadata *book*)))
  (setf (metadata-identifier md) "uuid:0001")
  (setf (metadata-title md) "The book title")
  (setf (metadata-language md) "en")
  (setf (metadata-modified-timestamp md) "Ons 17 Jul 2013 22:10:46 CEST")
  (setf (metadata-creator md) "Lisp Y. Guy"))

;; Write everything to disk
(write-epub *book* "my-book.epub")
