package org.podval.fop.mathjax

import java.io.{InputStream, StringBufferInputStream}

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.fop.util.UnclosableInputStream
import org.podval.fop.xml.Xml
import org.w3c.dom.svg.SVGDocument

object Svg {

  object Namespace extends org.podval.fop.xml.Namespace(uri = "http://www.w3.org/2000/svg")

  val mimeType: String = "image/svg+xml"

  def fromString(what: String): SVGDocument = {
    val in: InputStream = new UnclosableInputStream(new StringBufferInputStream(what))
    val length: Int = in.available
    in.mark(length + 1)
    svgFactory.createSVGDocument(null, in)
  }

  private lazy val svgFactory: SAXSVGDocumentFactory = {
    // org.apache.batik.dom.util.SAXDocumentFactory sets up a default SAXParserFactory
    // in a class initializer, before it can be overridden in a constructor,
    // so the default should at least implement properties it sets up.
    // Depending on the classpath (yuck!), this default can turn out to be Saxon's internal Alfredo, which doesn't.
    // I need to make sure it is Xerces.
    // I hate using system properties to communicate with code, but this seems the only way.
    // When I set it globally, I get
    //   Could not initialize class jdk.xml.internal.JdkXmlUtils
    // in plugin tests - on GitHub, but not locally (different version of the JDK?).
    // So, I bracket the invocation that triggers that code and restore whatever was there (if there was anything).
    val propertyName: String = classOf[javax.xml.parsers.SAXParserFactory].getName
    val original: String = System.getProperty(propertyName)
    System.setProperty(propertyName, Xml.saxParserFactoryName)
    val result = new SAXSVGDocumentFactory(Xml.saxParserName)
    if (original != null) System.setProperty(propertyName, original)
    result
  }
}
