# Publishing on the Web #

## Requirements and Old Solution ##

I'd like to be able to publish my remarks in both HTML and PDF (preferrably, also in EPUB).

One way to do that is to write in [DocBook](https://docbook.org/)
and use DocBook XSLT [stylesheets](https://github.com/docbook/xslt10-stylesheets)
executed by [Saxon](http://www.saxonica.com/products/products.xml) to process DocBook
into HTML, XSL-FO and EPUB, and [FOP](https://xmlgraphics.apache.org/fop/) to
further process XSL-FO to PDF.

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
UNLESS there is some XSLT solution which I can then add to the DocBook pipeline?

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

I still didn't figure out if it is possible to include generated tables in the published HTML:
{\% include-relative xxx.md \%} probably doesn't work on GitHub pages.
Cross-linking (<a name="anchor"></a> ... \[\](#anchor) probably does though.

The road from Markdown to PDF is not easy, even if I write all the formulas in LaTeX.
For instance, conversion of a recent book published as a series of
[blog posts](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)
reuired a lot of [steps](https://github.com/hmemcpy/milewski-ctfp-pdf/)
(and manual intervention).
An even more recent [book](https://plfa.github.io/), written natively in Jekyll,
doesn't have a PDF conversion, and, [according](https://github.com/plfa/plfa.github.io/issues/106)
to the author of the elaborate machinery that generates the book's website:
"This should be possible, and I have created similar setups in the past... but we're not currently
working on this."

Also, with Markdown I lose:
- structuring text into chapters and including them
- table of contents
- index
- footnotes
and all the advantages of semantic markup.


So, current decision (beginning of 2019) is:
1. continue with DocBook plugin (now in a separate repository),
with the goal of supporting multi-format generation with diagrams, fonts, program-generated tables etc.,
but with formulas as images.
2. Use GitHub pages' Markdown for short preliminary pieces.

# JEuclid FOP Plugin #

[Apache FOP](https://xmlgraphics.apache.org/fop/) can be extended to handle
specialized markup by using plugins.

One such plugin - JEuclid FOP - handles MathML in the XSL-FO files, which is what is needed
to use MathML in DocBook: XSLT stylesheets that generate XSL-FO from DocBook pass MathML through
untouched. That plugin uses [JEuclid](http://jeuclid.sourceforge.net/) for the actual rendering.
Bot were written by [Max Berger](https://github.com/maxberger).

Latest version of JEuclid and the FOP plugin - 3.1.9 - was released on 2010-02-12 and
targets FOP 0.95. It is available in Maven repositories (`net.sourceforge.jeuclid:jeuclid-fop:3.1.9`),
but attempts at using it in the DocBook plugin produce an exception:

```
java.lang.NoClassDefFoundError: Could not initialize class net.sourceforge.jeuclid.elements.JEuclidElementFactory
  at net.sourceforge.jeuclid.elements.generic.DocumentElement.createElement(DocumentElement.java:152)
  at com.icl.saxon.output.DOMEmitter.startElement(DOMEmitter.java:53)
```

I am not sure what is the exact cause of that: I am doing something wrong, JEuclid's and Gradle's
ideas about classpaths differ, some reflection is in play...

Fortunately, there exists a [fork](https://github.com/rototor/jeuclid) of JEuclid
by [Emmeran Seehuber](https://github.com/rototor) that brings the codebase up-to-date in some
respects: it now builds on JDK 9 and uses more current versions of some dependencies
(for example, Batik).

Unfortunately, that fork removed the FOP extension, since it wasn't useful for the project
(details are in the [discussion](https://github.com/danfickle/openhtmltopdf/issues/161) that
led to the fork).  

Fortunately, using unchanged original Java sources of the JEuclid plugin with the
forked JEuclid works! I guess FOP, which is currently at version 2.3,
still supports the original extension mechanism :)

Although FOP JEuclid plugin code works as is, I mage one change: I use explicit programmatic
configuration instead of leaving classpath provider-configuration crumbs for the the ServerLoader
(see JEuclidFopFactoryConfigurator class). The reason I did this (besides my general dislike
for magic) is: I need to be able to disable JEuclid based on the configuration of my plugin in
the Gradle build file, which isn't possible if it self-enables just by being on the classpath. 

Here are the names and contents of the provider-configuration files that JEuclid FOP plugin
had under `META-INF/services` (for informational purposes):

| Name                                                        | Contents                                                     |
|-------------------------------------------------------------|--------------------------------------------------------------|
| org.papache.fop.fo.ElementMapping                           | net.sourceforge.jeuclid.fop.JEuclidElementMapping            |
| org.apache.fop.render.XMLHandler                            | net.sourceforge.jeuclid.fop.JEuclidXMLHandler                |
|  org.apache.xmlgraphics.image.loader.spi.ImageConverter     | net.sourceforge.jeuclid.xmlgraphics.ImageConverterMathML2G2D |
|  org.apache.xmlgraphics.image.loader.spi.ImageLoaderFactory | net.sourceforge.jeuclid.xmlgraphics.ImageLoaderFactoryMathML |
|  org.apache.xmlgraphics.image.loader.spi.ImagePreloader     | net.sourceforge.jeuclid.xmlgraphics.PreloaderMathML          |


I should probably submit a pull request to restore FOP plugin in the forked JEuclid,
but I am getting compile errors in ConverterRegistry (talk about ServiceLoader!):
```java
  final Iterator<ConverterDetector> it = Service.providers(ConverterDetector.class);
```
```
  incompatible types:
   java.util.Iterator<java.lang.Object> cannot be converted to
   java.util.Iterator<net.sourceforge.jeuclid.converter.ConverterDetector>
```
Besides, maybe I am the last developer even trying to use JEuclid for FOP?
(How do all the XML editors - and they all ship it - make it work?)


# Gradle DocBook Plugin #

## Motivation ##

Inspired by the docbkx Maven plugin: http://docbkx-tools.sourceforge.net/docbkx-samples/manual.html
Blog post.
FOP version and lack of updates from docbkx.
Inspired by a Gist by Aristedes Maniatis: https://gist.github.com/ari/4156d967d54289f4abf6
The need for Gradle plugin (DRY).

## Saxon version ##

DocBook XSLT stylesheets are in XSLT 1.0; rewrite in XSLT 2.0 did not yet release anything (link).

On http://saxon.sourceforge.net/, it says:
   "even if your stylesheets only require XSLT 1.0, it is probably better to run them under Saxon 9.x"

Saxon-HE had support for XSLT 1.0 up to v9.7, then dropped it, then - allegedly - reinstated it
by popular demand in v9.8.0.7. It doesn't seem fixed to me; when I use v9.8.0-10, I see a lot of
   "Static error... XPST0008: Variable... has not been declared (or its declaration is not in scope),
 but when I use v9.7.0-21 - I do not.

But it still doesn't work; I get (from DocBook XSLT):
   Don't know how to chunk with Saxonica
   Error at char 16 in xsl:value-of/@select on line 84 column 63 of chunker.xsl:
   XTMM9000: Processing terminated by xsl:message at line 84 in chunker.xsl

 It seems that the extensions that DocBook XSLT stylesheets used to produce multiple output files are
 not supported even by Saxon-HE versions that supports XSLT 1.0.

 I had to downgrade to a version of Saxon that is an XSLT 1.0 processor (with the needed extensions) :(
 That seems to be v6.5.5, but it is not available from Maven Central or JCenter, so I had to go with v6.5.3 :(


## DocBook XSLT version ##

v1.79.2 has been out for a while, but doesn't seem to have made it into Maven repositories,
 so I use v1.79.1

Make version overridable in the project using this extension in docbook{ xslVersion = ...}
and avoid "can't change after... resolved" - like 'zinc' for scala-plugin.


## JEuclid version ##

FOP does not handle MathML; it needs jEuclid plugin
net.sourceforge.jeuclid:jeuclid-core:3.1.9
net.sourceforge.jeuclid:jeuclid-fop:3.1.9  (both seem to be available from the Maven repositories!)

FOP installation... did not recognize the jeuclid's jar-files (version 3.1.9) in /usr/share/fop (or some subfolder /lib).
I was able to get it to work by modifying /usr/bin/fop which is actually just a shell script wrapper. The modified parts are as follows:
...
find_jars /usr/share/fop/fop-hyph.jar
find_jars /usr/share/fop/jeuclid-core-3.1.9.jar
find_jars /usr/share/fop/jeuclid-fop-3.1.9.jar
find_jars fop
...   

NO LONGER SUPPORTED; DOESN"T WORK WITH Java 9 (Link to the note from the author!)

https://print-css.rocks/blog/2016-10-06/integrating-mathjax.html

Look into:
https://github.com/vivliostyle/vivliostyle.js
https://sourceforge.net/projects/dblatex/
