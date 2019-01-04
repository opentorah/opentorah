---
---
# Publishing on the Web #

## Requirements and Old Solution ##

I'd like to be able to publish my remarks in both HTML and PDF (preferrably, also in EPUB).

One way to do that is to write in [DocBook](https://docbook.org/)
and use DocBook XSLT [stylesheets](https://github.com/docbook/xslt10-stylesheets)
executed by [Saxon](http://www.saxonica.com/products/products.xml) to process DocBook
into HTML, XSL-FO and EPUB, and [FOP](https://xmlgraphics.apache.org/fop/) to
futher process XSL-FO to PDF.

I'd like to be able to insert a formula into the remarks now and then.

Standard modern way of doing that is [MathML](https://www.w3.org/Math/).
Although native MathML support in the web browsers is uneven, actively developed JavaScript
library [MathJax](https://www.mathjax.org/) takes care of all browsers (but probably not
all EPUB readers). It also supports formulas in LaTeX.

FOP does not support MathML natively, but can be configured to use a plug-in that does -
[JEuclid](http://jeuclid.sourceforge.net/jeuclid-fop/).

I need to include an occasional diagram. Didn't try it yet, but I presume that browsers know
how to handle SVG; not so sure about FOP.
 
I need to include in the published document tables generated programmatically.

DocBook does support includes, but the workflow becomes even more complex. Thankfully,
there exists a [Maven](https://maven.apache.org/) DocBook
[plugin](https://github.com/mimil/docbkx-tools) that automates a lot of it.   

Here is my old [blog post](http://blog.dub.podval.org/2011/11/publishing-papers-on-web.html)
with the technical details.

## Current State of Affairs ##

Things changed since then.

FOP became better (with fonts, among other things).

It became harder to find a free version of Saxon.

JEuclid is no longer maintained, doesn't integrate with current FOP,
and doesn't work on Java 9 [at all](https://github.com/danfickle/openhtmltopdf/issues/161).
So, there isn't non-commercial way from DocbBook with MathML to PDF at this point :(

I followed the flow from Maven to [Gradle](https://gradle.org/); there is no Gradle DocBook plugin,
although there are some scripts floating around. Since these scripts are not as full-featured as
the Maven plugin, and since they do not always work with current versions of FOP and friends, I
started working on a Gradle equivalent of the original Maven plugin, but even if I finished it,
there seems to be no way to get formulas into the PDF starying from DocBook.

I am not up-to-date on the HTML5/CSS publishing, and I am not ready to write everything in
LaTeX because of an occasional formula...

This sorry state of affairs contributed to the fact that I didn't publish any of my notes over the
past few years. Out of desperation, I decided to look into publishing on
[GitHub](https://github.com/), thinking that publishing something in HTML is better than
publishing nothing in HTML and PDF :)

I don't really want to use GitHub [Wikis](https://help.github.com/articles/about-github-wikis/) -
and I don't have to: GitHub [Pages](https://pages.github.com/) (where my project's website is hosted
already) has reasonably full [Jekyll](https://jekyllrb.com/)
[MarkDown](https://daringfireball.net/projects/markdown/) to HTML workflow.

MarkDown editing is available in both [IntelliJ Idea](https://www.jetbrains.com/idea/)
and [Oxygen](https://www.oxygenxml.com/).

It is possible to add MathJax header incantations to the pages that have formulas.

I still didn't figure out if it is possible to include generated tables in the published HTML.
(When I do, I should publish this note as an update to my original blog post :))

The road from Mardown to PDF is not easy, even if I write all the formulas in LaTeX.
For instance, conversion of a recent book published as a series of
[blog posts](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)
reuired a lot of [steps](https://github.com/hmemcpy/milewski-ctfp-pdf/)
(and manual intervention).
An even more recent [book](https://plfa.github.io/), written natively in Jekyll,
doesn't have a PDF conversion, and, [according](https://github.com/plfa/plfa.github.io/issues/106)
to the author of the elaborate machinery that generates the book's website:
"This should be possible, and I have created similar setups in the past... but we're not currently
working on this."

So, current decision (beginning of 2019) is: give up on the PDF for now, give up on the
rich semantic markup of DocBook, write and publish in MarkDown (at least it's not LaTeX!),
and hope that by the time I am "done", automated PDF conversion will become available :)


## DocBook Issues ##

I am draining the open DockBook- and DockBook plugin-related issues, since this switch obviates them.
Since I am not completely sure that it is irreversible, I am recording them here for now.

_Footnotes_ DocBook stylesheets render each footnote on a separate line; when there are many of
them, this doesn't look good. Is there a way to render footnotes as a continuous block?

_Honorifics_ DocBook stulesheets render honorifics in bibliography with dots after them,
which doesn't look good for 'Rabbi' :) It can be removed
(as in [chumashquestions.org](http://www.chumashquestions.org/)), but has then to be typed in
for all other honorifics.
 
_Edition_ In bibliography, when I give "edition" as "3rd", it is printed as "3rd", not "3rd edition".
Am I supposed to spell it out everywhere?!

_Numbers_ Numbers (degrees) seem to be printed lower than letters in the PDF output.

_Links_ Links (e.g., bibliorefs) in PDF output are clickable, but it is not obvious.
Can I persuade DocBook stylesheets to make them underlined with blue or something?

_Fonts_ Hebrew and Russian seem to work in PDF when it is generated by Oxygen, but not Maven?
Set body and title font families to something that has glyphs for English, Cyrillic and Hebrew
(DejaVu Sans?). The fonts get embedded in the PDF document.

_Revision_ Add date of revision to the title page.
