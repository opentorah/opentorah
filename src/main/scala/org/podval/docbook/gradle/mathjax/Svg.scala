package org.podval.docbook.gradle.mathjax

import java.io.{InputStream, StringBufferInputStream}

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.fop.image.loader.batik.PreloaderSVG
import org.apache.fop.util.UnclosableInputStream
import org.w3c.dom.svg.SVGDocument

object Svg {

  object Namespace extends org.podval.docbook.gradle.xml.Namespace(uri = "http://www.w3.org/2000/svg")

  val mimeType: String = "image/svg+xml"

  def fromString(what: String): SVGDocument = {
    val in: InputStream = new UnclosableInputStream(new StringBufferInputStream(what))
    val length: Int = in.available()
    in.mark(length + 1)
    svgFactory.createSVGDocument(null, in)
  }

  private val svgFactory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(PreloaderSVG.getParserName)
}
