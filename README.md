## Motivation

Inspired by the docbkx Maven plugin: http://docbkx-tools.sourceforge.net/docbkx-samples/manual.html
Blog post.
FOP version and lack of updates from docbkx.
Inspired by a Gist by Aristedes Maniatis: https://gist.github.com/ari/4156d967d54289f4abf6
The need for Gradle plugin (DRY).

## Saxon version

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


## DocBook XSLT version

v1.79.2 has been out for a while, but doesn't seem to have made it into Maven repositories, so I use v1.79.1

Make version overridable in the project using this extension in docbook{ xslVersion = ...}
and avoid "can't change after... resolved" - like 'zinc' for scala-plugin.


## JEuclid version

FOP does not handle MathML; it needs jEuclid plugin
net.sourceforge.jeuclid:jeuclid-core:3.1.9
net.sourceforge.jeuclid:jeuclid-fop:3.1.9  (both seem to be available from the Maven repositories!)


## TODO

Suppress the logs from Saxon:
     // suppress output from the XSLT transforms - unless running with '-d' or '-i'.
     switch (project.gradle.startParameter.logLevel) {
       case LogLevel.DEBUG:
       case LogLevel.INFO:
       break;
       default:
         logging.captureStandardOutput(LogLevel.INFO)
       logging.captureStandardError(LogLevel.INFO)
     }


FOPp installation... did not recognize the jeuclid's jar-files (version 3.1.9) in /usr/share/fop (or some subfolder /lib).
I was able to get it to work by modifying /usr/bin/fop which is actually just a shell script wrapper. The modified parts are as follows:
...
find_jars /usr/share/fop/fop-hyph.jar
find_jars /usr/share/fop/jeuclid-core-3.1.9.jar
find_jars /usr/share/fop/jeuclid-fop-3.1.9.jar
find_jars fop
...   

Switch to Zinc v1 - when it actually works!
  'com.typesafe.zinc:zinc:0.3.15' -> "org.scala-sbt:zinc-core_${scalaVersionMajor}:1.1.5"
