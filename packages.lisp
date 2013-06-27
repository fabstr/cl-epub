(defpackage :html-gen
  (:use :common-lisp)
  (:export :html
	   :xml
	   :generate-html
	   :generate-xml))

(defpackage :epub
  (:use :common-lisp
	:html-gen)
  (:export :write-mimetype
	   :write-container-xml
	   :write-content
	   :write-package-document
	   :create-metadata))
