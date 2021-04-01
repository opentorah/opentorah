package org.opentorah.fop

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.fop.util.UnclosableInputStream
import org.opentorah.xml.{Dialect, Namespace, Xerces}
import org.w3c.dom.svg.SVGDocument
import java.io.{InputStream, StringBufferInputStream}

object Svg extends Dialect {

  override val namespace: Namespace = Namespace(uri = "http://www.w3.org/2000/svg", prefix = "svg")

  override val mimeType: String = "image/svg+xml"

  private lazy val svgFactory: SAXSVGDocumentFactory = {
    // org.apache.batik.dom.util.SAXDocumentFactory sets up a default SAXParserFactory
    // in a class initializer, so the default should at least implement properties it sets up.
    // And of course it uses the magical service discovery stuff, so depending on the classpath (yuck!),
    // this default can turn out to be Saxon's internal "aelfred" parser, which does not.
    // And although the actual parser used can be supplied as a constructor parameter (and I do supply it),
    // by then alarming stacktrace already escaped into the logs...
    // I need to make sure that SAXDocumentFactory chooses Xerces as its default saxFactory.
    // I hate using system properties to communicate with code, but this seems to be the only way.
    // When I set it globally, I get
    //   Could not initialize class jdk.xml.internal.JdkXmlUtils
    // in plugin tests - on GitHub, but not locally (different version of the JDK?).
    // So, I bracket the invocation that triggers that code and restore whatever was there (if there was anything).
    // Also, since my MathJax support is not the only source of the SVG files that FOP can encounter,
    // I provide a method forceXerces() that can be used to force this issue before FOP runs.
    val propertyName: String = Xerces.saxParserFactoryProperty
    val original: String = System.getProperty(propertyName)
    System.setProperty(propertyName, Xerces.saxParserFactoryName)
    val result = new SAXSVGDocumentFactory(Xerces.saxParserName)
    if (original != null) System.setProperty(propertyName, original)
    result
  }

  def forceXerces(): Unit = svgFactory

  def fromString(what: String): SVGDocument = {
    // TODO use StringReader instead of the deprecated StringBufferInputStream;
    // mark() call probably does nothing and should then be removed...
    val in: InputStream = new UnclosableInputStream(new StringBufferInputStream(what))
    val length: Int = in.available
    in.mark(length + 1)
    svgFactory.createSVGDocument(null, in)
  }
}
