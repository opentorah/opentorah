/*
 * Inspired by a Gist by Aristedes Maniatis
 * https://gist.github.com/ari/4156d967d54289f4abf6
 */

## FOP version

## Saxon version

DocBook XSLT stylesheets are in XSLT 1.0; rewrite in XSLT 2.0 did not yet release anything.

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


## DocBook XSLT version

v1.79.2 has been out for a while, but doesn't seem to have made it into Maven repositories, so I use v1.79.1


// Task classes can not be 'final' - Gradle needs to create a proxy...

## JEuclid version

FOP does not handle MathML; it needs jEuclid plugin
net.sourceforge.jeuclid:jeuclid-core:3.1.9

jeuclid-fop is not available in Maven central repository and must be made available locally
net.sourceforge.jeuclid:jeuclid-fop:3.1.9


## Zinc version
'com.typesafe.zinc:zinc:0.3.15'
 // TODO - when will it work? "org.scala-sbt:zinc-core_${scalaVersionMajor}:1.1.5"
 
 
 
// TODO check that input and stylesheet files exist
// TODO auto-remove ".xml" from the input file name?


// TODO track down and suppress network IO!


Add EPUB support

Add SVG support



    // TODO fix FOP configuration to not get
    //   "font directory /home/dub/Projects/calendar/calendar/paper/src/main/fop/fop.xconf could not be found."
    // Also, strict?
