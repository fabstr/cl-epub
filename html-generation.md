# How to use the html-generation

The html generation DSL is written to allow humans to write html (and xml) code
in a lispy way in the current lexical environment.

The html generation takes up O(n) space.

## Table of contents:
- [A simple example](#a-simple-example)
- [An attribute](#an-attribute)
- [Multiple attributes](#multiple-attributes)
- [Two elements](#two-elements)
- [Child elements](#child-elements)
- [A larger example](#a-larger-example)
- [Variables](#variables)
- [More formal grammar](#more-formal-grammar)
- [The difference between html and xml](#the-difference-between-html-and-xml)
- [generate-html and generate-xml](#generate-html-and-generate-xml)
- [Creating an xml declaration](#creating-an-xml-declaration)

## A simple example
<a id="a-simple-example"></a>
```lisp
(html (:p () "Hello, World!"))
```
which is translated to
```
"<p>Hello, World!</p>"
```

## An attribute
<a id="an-attribute"></a>
```lisp
(html (:a ((:href "http://www.example.com")) "Example"))
```
is translated to
```
"<a href=\"http://www.example.com\">Example</a>"
```
The attributes is defined as *a list of lists*, where each inner list is a pair.
As we see in the example, the car should be a keyword (or a variable that can be
printed to a string) and the cadr should be a string (or something that can be
printed to a string).

## Multiple attributes
<a id="multiple-attributes"></a>
```lisp
(html (:a ((:href "http://www.example.com") (:id "the-first-link"))
          "target"))
```
is translated to
```
"<a href=\"example.com\" id=\"the-first-link\">target</a>"
```

## Two elements
<a id="two-elements"></a>
```lisp
(html (:h1 () "Header")
      (:p () "a paragraph"))
```
is translated to
```
"<h1>Header</h1><p>a paragraph</p>"
```

Since xml parses shouldn't mind (lack of) indentation and separation the
elements are printed in a continious string.

## Child elements
<a id="child-elements"></a>
```lisp
(html (:div ()
            (:p () "a text")
            (:p () "with many paragraphs")))
```
is translated to
```
"<div><p>a text</p><p>with many paragraphs</p></div>"
```
Child elements can be inserted in a natural way.


## A larger example
<a id="a-larger-example"></a>
```lisp
(html (:html ()
			 (:head ()
				    (:title () "title")
				    (:meta ((:these "are") (:meta "tags")))
				    (:link ((:rel "stylesheet") (:type "text/css")
					        (:href "css.css"))))
	         (:body ()
				    (:div ((:id "box"))
				          (:h1 () "header")
				          (:p () "paragraph"))
	                (:div ((:id "foot"))
				          (:p () "this is a foot")))))
```
is translated to
```
"<html><head><title>title</title><meta these=\"are\" meta=\"tags\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"css.css\" /></head><body><div id=\"box\"><h1>header</h1><p>paragraph</p></div><div id=\"foot\"><p>this is a foot</p></div></body></html>"
```
Or, in human readable html:
```html
<html>
  <head>
    <title>title</title>
    <meta these="are" meta="tags" />
    <link rel="stylesheet" type="text/css" href="css.css" />
  </head>
  <body>
    <div id="box">
      <h1>header</h1>
      <p>paragraph</p>
    </div>
    <div id="foot">
      <p>this is a foot</p>
    </div>
  </body>
</html>
```

## Variables
<a id="variables"></a>
The html is generated within the current lexical environment:
```lisp
(let ((par "this is a paragraph")
      (key "href")
      (value "http://www.example.com")
      (target "this link goes to example.com"))
  (html (:p () par)
        (:a ((key value)) target)))
```
is the same as
```lisp
(html (:p () "this is a paragraph")
      (:a (("href" "http://www.example.com")) "this link goes to example.com"))
```
and is translated to
```
"<p>this is a paragraph</p><a href=\"http://www.example.com\">this link goes to example.com</a>"
```

## More formal grammar
<a id="more-formal-grammar"></a>
Within the html environment, the list is converted to a html string if the
following criteria are met:
- We are evaluating a list
- The car of the list is a keyword
- The cadr of the list is a list

Thus, these lists are valid for html generation (they are "html statements"):
```lisp
(:p () "foo")
(:a ((:href "foo")) "bar")
```

But these are not:
```lisp
"foo"
(:p "foo" "bar")
(format nil "foo")
```
(Not a list, cadr not a list, first not a keyword.)

However, the html generation is smart enough to handle some of these cases:
```lisp
(html (:p () "foo")
      "this is a string"
      (:p () "bar"))
```
just returns
```
"<p>foo</p>this is a string<p>bar</p>"
```

More specifically, if the current statement (what we are evaluating, for
example "this is a string" or (:p () "foo")) is not a html statement it is
passed through format:
```lisp
(format stream "~a" "this is a string")
```
Where stream is bound via *with-output-to-string*.

As a consequense functions calls can be used:
```lisp
(defun my-function (arg)
  (format nil "You called with arg=~s" arg))
(html (:p () (my-function "foo")))
```

is transformed into
```
"<p>You called with arg=\"foo\"</p>"
```

## The difference between html and xml
<a id="the-difference-between-html-and-xml"></a>
In the xml environment, void elements are given close tags when they are not in
the html environment:
```lisp
(xml (:meta ((:foo "bar"))))
(html (:meta ((:foo "bar"))))
```
is translated to (respectively)
```
"<meta foo=\"bar\"></meta>"
"<meta foo=\"bar\" />"
```

## generate-html and generate-xml
<a id="generate-html-and-generate-xml"></a>
If you already have an open writing stream (for example writing to a file) it
is possible to avoid O(n) extra storage space by generating the html directly
to the stream. The following two snippets have the same effect:
```lisp
(with-open-file (str "my-file.html" :direction :output :if-exists :supersede)
  (generate-html (str)
    (:html ()
	       (:head ()
                  (:title () "title"))
           (:body ()
                  (:p () "some text")))))
```

```lisp
(with-open-file (str "my-file.html" :direction :output :if-exists :supersede)
  (format str (html (:html ()
                           (:head ()
                                  (:title () "title"))
                           (:body ()
                                  (:p () "some text")))))
```

Actually, html are just a wrapper for generate-html (the same thing goes for
xml):
```lisp
(defmacro html (&rest statements)
  (with-gensyms (stream)
    `(with-output-to-string (stream)
       (generate-html (,stream) ,@statements))))
```

## Creating an xml declaration
<a id="creating-an-xml-declaration"></a>
An xml declaration such as
```xml
<?xml this="is" something="else"?>
```
is created by this:
```lisp
(xml-declaration (:this "is") (:something "else"))
```
