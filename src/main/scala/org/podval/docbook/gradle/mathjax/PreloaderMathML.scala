package org.podval.docbook.gradle.mathjax

import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import org.apache.xmlgraphics.image.loader.{ImageContext, ImageInfo}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImagePreloader, ImageXMLDOM}
import org.podval.docbook.gradle.Namespace
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.Document

final class PreloaderMathML(fopPlugin: MathJaxFopPlugin) extends AbstractImagePreloader {

  // NOTE: JEuclid's MathML preloader also parses MathML from a stream; do I need this?
  override def preloadImage(
    uri: String,
    src: Source,
    context: ImageContext): ImageInfo =
  {
    val document: Document = src.asInstanceOf[DOMSource].getNode.asInstanceOf[Document]

    if (!Namespace.MathML.is(document)) null else  {
      val svgDocument: SVGDocument = fopPlugin.typeset(document)

      val result: ImageInfo = new ImageInfo(uri, Namespace.SVG.mimeType)

      result.setSize(Sizes(svgDocument).getImageSize(context.getSourceResolution))

      // Stash the result to avoid typesetting again:
      result
        .getCustomObjects.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
        .put(ImageInfo.ORIGINAL_IMAGE, new ImageXMLDOM(result, svgDocument, Namespace.SVG.uri))

      result
    }
  }
}
