package org.podval.docbook.gradle.mathjax

import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import org.apache.batik.util.SVGConstants
import org.apache.xmlgraphics.image.loader.{ImageContext, ImageInfo}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImagePreloader, ImageXMLDOM}
import org.apache.xmlgraphics.util.MimeConstants
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.Document

final class PreloaderMathML(mathJax: MathJax) extends AbstractImagePreloader {

  // NOTE: JEuclid's MathML preloader also parses MathML from a stream; do I need this?
  override def preloadImage(
    uri: String,
    src: Source,
    context: ImageContext): ImageInfo =
  {
    val document: Document = src.asInstanceOf[DOMSource].getNode.asInstanceOf[Document]

    if (!isMathML(document)) null else  {
      val svgDocument: SVGDocument = FopPlugin.mathML2SVG(document, mathJax)
      val sizes: Sizes = Sizes(svgDocument)

      val result: ImageInfo = new ImageInfo(uri, MimeConstants.MIME_SVG)

      result.setSize(sizes.getImageSize(context.getSourceResolution))

      // The whole image had to be loaded for this, so keep it
      result.getCustomObjects.asInstanceOf[java.util.Map[AnyRef, AnyRef]].put(ImageInfo.ORIGINAL_IMAGE,
        new ImageXMLDOM(result, svgDocument, SVGConstants.SVG_NAMESPACE_URI))

      result
    }
  }

  private def isMathML(document: Document): Boolean =
    document.getDocumentElement.getNamespaceURI == FopPlugin.MathMLNameSpace
}
