# How to create an epub book

## Table of contents
 - [What you need](#what-you-need)
 - [How to put the pieces together](#how-to-put-the-pieces-together)
   - [Creating a paragraph](#creating-a-paragraph)
   - [Creating a section](#creating-a-section)
   - [Adding section to the book](#adding-section-to-the-book)
   - [Adding the metadata](#adding-the-metadata)
   - [Add the pieces together](#add-the-pieces-together)
 - [How to save as epub](#how-to-save-as-epub)
 - [Links](#links)

Please don't use defparameter when not appropriate.

## What you need
<a id="what-you-need"></a>

1. The paragraphs for the book.
2. The sections for the book (which are made up of the paragraphs)
3. The required elements for the metadata:
   - A unique identifier, the title of the book
   - The language of the book
   - A timestamp on when the content was last modified

## How to put the pieces together
<a id="how-to-put-the-pieces-together"></a>
### Creating a paragraph
<a id="creating-a-paragraph"></a>
To create a paragraph two things are needed: the text and the index of the
paragraph. The index specifies in what order they are read from the section, ie
a section that comes before another should have a lower index.

The index should be a simple integer and text should be in html, for example in
a p tag.

Both :index and :text must be supplied when creating a paragraph.

#### Examples
How to create a paragraph:

```lisp
(defparameter *par1*
  (make-instance
	'Paragraph
    :index 0
    :text "<p>This is the paragraph. Please note the p element.</p>"))
```

And another:

```lisp
(defparameter *par2*
  (make-instance
	'Paragraph
    :index 1
	:text "<p>Is there any need for multiple paragraphs?</p>"))
```

### Creating a section
<a id="creating-a-section"></a>
Each section consists of one or more paragraphs and must also have a index. A
section can be added to the table of contents.

When the section has been intansiated, the paragraps should be added.

If the section is to be in the table of contents, it should have a title as
described [here][1].

#### Examples
We assume that the paragraphs from above are bound to \*par1\* and \*par2\*.

Create a section and add the paragraphs:

```lisp
(defparameter *section1* (make-instance 'Section :index 0))
(add-paragraph *section1* *par1*)
(add-paragraph *section1* *par2*)
```

A section that is to be in the table of contents:

```lisp
(defparameter *section2*
  (make-instance
	'Section
    :index 1
    :add-to-toc-p t
	:title "<a href=\"Content.xhtml#par1\">The second section</a>"))
(add-paragraph *section2* *par1*)
(add-paragraph *section2* *par2*)
```

### Adding section to the book
<a id="adding-section-to-the-book"></a>
Assuming we have \*section1\*, \*section2\* and \*book\*, just do

```lisp
(add-section *book* *section1*)
(add-section *book* *section2*)
```

### Adding the metadata
<a id="adding-the-metadata"></a>
There are four metadata elements that are required for an epub book:

 - The unique identifier
 - The title of the book
 - The language
 - A timestamp of when the book was last modified

To add the the metadata, use the accessors of the metadata class.

In addition there are many other (though optional) elements:
*identifier, title, language, modifiedtimestamp, meta, link, contributor,
coverage, creator, date, description, format, publisher, relation, rights,
source, subject, type*

More information about the contents of these elements:

 - [The metadata element][2]
 - [The optional elements][3]

#### Examples
We assume we have an Epub object bound to \*book\*.

To set the required metadata elements:

```lisp
(let ((metadata (epub-metadata *book*)))
  (setf (metadata-identifier metadata) "my-uuid")
  (setf (metadata-title) "My super awesome book")
  (setf (metadata-language) "en")
  (setf (metadata-modified-timestamp) "Ons 17 Jul 2013 22:10:46 CEST"))
```

To add an author to the book:

```lisp
(setf (metadata-creator (epub-metadata *book*)) "The Author")
```

Please see the documentation for the Metadata class for the rest of the
elements.

### Add the pieces together
<a id="add-the-pieces-together"></a>
First you need to create an epub object:

```lisp
(defparameter *book* (make-instance 'Epub))
```

Then add the sections:

```lisp
(add-section *book* *section1*)
(add-section *book* *section2*)
```

Third add the metadata **to be implemented**

## How to save as .epub
<a id="how-to-save-as-epub"></a>
Just call

```lisp
(write-epub *book* "path/to/book.epub")
```

and your done!

## Links
<a id="links"></a>

- [The title of a section][1]
- [The metadata element][2]
- [The optional metadata elements][3]
- [The epub specification][4]

[1]: http://www.idpf.org/epub/30/spec/epub30-contentdocs.html#sec-xhtml-nav-def "The title of a section"
[2]: http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-metadata-elem "The metadata element"
[3]: http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-opf-dcmes-optional "The optional elements"
[4]: http://www.idpf.org/epub/30/spec/ "The epub specification"
