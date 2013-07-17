# How to create an epub book:

## Table of contents
1
2 ..
3 .

### How to read the examples:
Please don't use defparameter when not appropriate.


## What you need
[what-you-need]: "What you need"
1. The paragraphs for the book.
2. The sections for the book (which are made up of the paragraphs)
3. The required elements for the metadata:
 - A unique identifier, the title of the book
 - the language of the book
 - a timestamp on when the content was last modified

## How to put the pieces together
[put-pieces-together]: "How to put the pieces together"

### Creating a paragraph
[creating-a-paragraph]: "How to create a paragraph"
To create a paragraph two things are needed: the text and the index of the
paragraph. The index specifies in what order they are read from the section, ie
a section that comes before another should have a lower index.

The index should be a simple integer and text should be in html, for example in
a p tag.

Both :index and :text must be supplied when creating a paragraph.

#### Examples
How to create a paragraph:

```lisp
(defparameter *par1* (make-instance 'Paragraph :index 0 :text "<p>This is the paragraph. Please note the p element.</p>"))
```

And another:

```lisp
(defparameter *par2* (make-instance 'Paragraph :index 1 :text "<p>Is there any need for multiple paragraphs?</p>"))
```

### Creating a section
[creating-a-section]: "How to create a section"
Each section consists of one or more paragraphs and must also have a index. A
section can be added to the table of contents.

When the section has been intansiated, the paragraps should be added.

If the section is to be in the table of contents, it should have a title as
described [here](http://www.idpf.org/epub/30/spec/epub30-contentdocs.html#sec-xhtml-nav-def).

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
(defparameter *section2* (make-instance 'Section :index 0 :add-to-toc-p t :title "<a href=\"Content.xhtml#par1\">The second section</a>"))
(add-paragraph *section2* *par1*)
(add-paragraph *section2* *par2*)
```

### Adding section to the book
[add-section-to-book]: "How to add a section to the book"
Assuming we have \*section1\*, \*section2\* and \*book\*, just do

```lisp
(add-section *book* *section1*)
(add-section *book* *section2*)
```

### Adding the metadata
[add-metadata]: "How to add the metadata"
There are four metadata elements that are required for an epub book:

 - The unique identifier
 - The title of the book
 - The language
 - A timestamp of when the book was last modified

In addition there are many other (though optional) elements:

 - identifier
 - title
 - language
 - modified-timestamp
 - meta
 - link
 - contributor
 - coverage
 - creator
 - date
 - description
 - format
 - publisher
 - relation
 - rights
 - source
 - subject
 - type

More information about the contents of these elements:

 - [The metadata element](http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-metadata-elem)
 - [The optional elements](http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-opf-dcmes-optional).

#### Examples
We assume we have an Epub object bound to \*book\*.

**to be written (and implemented)**

### Add the pieces together
[add-pieces-together]: "How to make it an epub file"
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
[save-as-epub]: "How to write everything to disk"
Just call

```lisp
(write-epub *book*)
```
