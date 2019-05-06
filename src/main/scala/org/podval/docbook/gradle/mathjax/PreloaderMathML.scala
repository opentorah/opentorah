package org.podval.docbook.gradle.mathjax

import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import org.apache.batik.util.SVGConstants
import org.apache.xmlgraphics.image.loader.{ImageContext, ImageInfo, ImageSize}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImagePreloader, ImageXMLDOM}
import org.apache.xmlgraphics.util.{MimeConstants, UnitConv}
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.Document

final class PreloaderMathML(mathJax: MathJax) extends AbstractImagePreloader {

  override def preloadImage(
    uri: String,
    src: Source,
    context: ImageContext): ImageInfo =
  {
    val mathmlDocument = src.asInstanceOf[DOMSource].getNode.asInstanceOf[Document]

    if (!FopPlugin.isMathML(mathmlDocument)) null else  {
      val svgDocument: SVGDocument = FopPlugin.mathML2SVG(mathmlDocument, mathJax)

      val sizes: FopPlugin.Sizes = FopPlugin.getSizes(svgDocument)

      val sourceResolution: Float = context.getSourceResolution
      val scale: Float = UnitConv.IN2PT / sourceResolution

      val size: ImageSize = new ImageSize
      size.setSizeInMillipoints(
        FopPlugin.toMilliPoints(sizes.width * scale),
        FopPlugin.toMilliPoints(sizes.height * scale)
      )
      size.setBaselinePositionFromBottom(FopPlugin.toMilliPoints(sizes.descent * scale))

      size.setResolution(sourceResolution)
      size.calcPixelsFromSize()

      val imageInfo: ImageInfo = new ImageInfo(uri, MimeConstants.MIME_SVG)
      imageInfo.setSize(size)

      // The whole image had to be loaded for this, so keep it
      val image: ImageXMLDOM = new ImageXMLDOM(imageInfo, svgDocument, SVGConstants.SVG_NAMESPACE_URI)
      imageInfo.getCustomObjects.asInstanceOf[java.util.Map[AnyRef, AnyRef]].put(ImageInfo.ORIGINAL_IMAGE, image)

      imageInfo
    }
  }
}
