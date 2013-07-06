(defclass Section ()
  ((index
    :initarg :index
    :initform (error "Must supply an index for the section.")
    :documentation "The index of the element. The index is used for sorting the
sections and define the reading order, ie a section with a low index will be
placed before a section with a higher index. index should be an integer."
    :reader section-index)
   (add-to-toc-p
    :initarg :add-to-toc-p
    :initform nil
    :documentation "T if the section is to be added to the table of contents.
Else nil. If t, a title must be specified."
    :reader section-add-to-toc-p)
   (title
    :initarg :title
    :initform ""
    :documentation "The title of the section, to be put in the table of
contents. The title should contain html as defined for the topmost li element in
http://www.idpf.org/epub/30/spec/epub30-contentdocs.html#sec-xhtml-nav-def."
    :reader section-title)
   (paragraphs
    :initarg :paragraphs
    :initform (make-hash-table)
    :documentation "A list of the paragraphs in the section. Each paragraph is
an instance of Paragraph."
    :accessor section-paragraphs)))

(defclass Paragraph ()
  ((index
    :initarg :index
    :initform (error "Must have an id for the paragraph.")
    :documentation "The index of the paragraph. When a Section is rendered to
html, the paragraphs are ordered according to the indexes."
    :reader paragraph-index)
   (text
    :initarg :text
    :initform (error "The paragraph must have text.")
    :documentation "The text of the paragraph. It should be in html form and
have <p> tags, ie text is \"<p>lorem lipsum...</p>\"."
    :reader paragraph-text)))

(defclass Item ()
  ((id
    :initarg :id
    :initform (error "Id must be supplied for the Item.")
    :reader item-id
    :documentation "The id attribute of the <item> element.")
   (href
    :initarg :href
    :initform (error "Href must be supplied for the Item.")
    :reader item-href
    :documentation "The href attribute of the <item> element.")
   (media-type
    :initarg :media-type
    :initform (error "Media-type must be supplied for the Item.")
    :reader item-media-type
    :documentation "The media-type attribute of the <item> element.")
   (fallback
    :initarg :fallback
    :initform nil
    :reader item-fallback
    :documentation "The fallback attribute of the <item> element.")
   (properties
    :initarg :properties
    :initform nil
    :reader item-properties
    :documentation "The properties attribute of the <item> element.")
   (media-overlay
    :initarg :media-overlay
    :initform nil
    :reader item-media-overlay
    :documentation "The media-overlay attribute of the <item> element.")))

(defclass Itemref ()
  ((idref
    :initarg :idref
    :initform (error "Idref must be supplied for the Itemref.")
    :reader itemref-idref
    :documentation "The idref attribute of the <itemref> element.")
   (linear
    :initarg :linear
    :initform nil
    :reader itemref-linear
    :documentation "The linear attribute of the <itemref> element.")
   (id
    :initarg :id
    :initform nil
    :reader itemref-id
    :documentation "The id attribute of the <itemref> element.")
   (properties
    :initarg :properties
    :initform nil
    :reader itemref-properties
    :documentation "The properties attribute of the <itemref> element.")))

(defclass Metadata ()
  ((identifier
    :initarg :identifier
    :initform :nil
    :accessor metadata-identifier
    :documentation "The identifier content of the metadata element.")
   (title
    :initarg :title
    :initform nil
    :accessor metadata-title
    :documentation "The title content of the metadata element.")
   (language
    :initarg :language
    :initform nil
    :accessor metadata-language
    :documentation "The language content of the metadata element.")
   (modified-timestamp
    :initarg :modified-timestamp
    :initform nil
    :accessor metadata-modified-timestamp
    :documentation "The modified-timestamp content of the metadata element.")
   (meta
    :initarg :meta
    :initform nil
    :accessor metadata-meta
    :documentation "The meta content of the metadata element.")
   (link
    :initarg :link
    :initform nil
    :accessor metadata-link
    :documentation "The link content of the metadata element.")
   (contributor
    :initarg :contributor
    :initform nil
    :accessor metadata-contributor
    :documentation "The contributor element of the metadata element")
   (coverage
    :initarg :coverage
    :initform nil
    :accessor metadata-coverage
    :documentation "The coverage content of the metadata element.")
   (creator
    :initarg :creator
    :initform nil
    :accessor metadata-creator
    :documentation "The creator content of the metadata element.")
   (date
    :initarg :date
    :initform nil
    :accessor metadata-date
    :documentation "The date content of the metadata element.")
   (description
    :initarg :description
    :initform nil
    :accessor metadata-description
    :documentation "The description content of the metadata element.")
   (format
    :initarg :format
    :initform nil
    :accessor metadata-format
    :documentation "The format content of the metadata element.")
   (publisher
    :initarg :publisher
    :initform nil
    :accessor metadata-publisher
    :documentation "The publisher content of the metadata element.")
   (relation
    :initarg :relation
    :initform nil
    :accessor metadata-relation
    :documentation "The relation content of the metadata element.")
   (rights
    :initarg :rights
    :initform nil
    :accessor metadata-rights
    :documentation "The rights content of the metadata element.")
   (source
    :initarg :source
    :initform nil
    :accessor metadata-source
    :documentation "The source content of the metadata element.")
   (subject
    :initarg :subject
    :initform nil
    :accessor metadata-subject
    :documentation "The subject content of the metadata element.")
   (type
    :initarg :type
    :initform nil
    :accessor metadata-type
    :documentation "The type content of the metadata element.")))

(defclass Epub ()
  ((sections
    :initarg :sections
    :accessor epub-sections
    :initform (make-hash-table)
    :documentation "The sections of the text of the epub book. The reading index
for the section is used as key in the hash table.")
   (toc
    :accessor epub-toc
    :initform (make-hash-table)
    :documentation "The table of contents of the epub book.")
   (metadata
    :accessor epub-metadata
    :initform (make-instance 'Metadata)
    :documentation
    "The metadata of the book, to be put in the package document.")
   (manifest
    :accessor epub-manifest
    ;; the id of each item is in string form, therefore #'equal must be the test
    :initform (make-hash-table :test #'equal)
    :documentation "The manifest, to be put in the package document.")
   (spine
    :accessor epub-spine
    ;; the spine has to be sorted and we don't know the size, therefore it is a
    ;; list
    :initform nil
    :documentation "The spine, to be put in the package document.")))
