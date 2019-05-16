package org.podval.docbook.gradle.mathjax

import java.awt.geom.Rectangle2D
import java.awt.{Dimension, Graphics2D, RenderingHints}

import org.apache.xmlgraphics.image.loader.{Image, ImageFlavor}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImageConverter, ImageGraphics2D, ImageXMLDOM}
import org.apache.xmlgraphics.java2d.Graphics2DImagePainter
import org.w3c.dom.Document
import org.w3c.dom.svg.SVGDocument

final class ImageConverterMathML2G2D(fopPlugin: MathJaxFopPlugin) extends AbstractImageConverter {
  override def getSourceFlavor: ImageFlavor = ImageFlavor.XML_DOM
  override def getTargetFlavor: ImageFlavor = ImageFlavor.GRAPHICS2D

  override def convert(src: Image, hints: java.util.Map[_, _]): Image = {
    val mathmlDocument: Document = src.asInstanceOf[ImageXMLDOM].getDocument
    val svgDocument: SVGDocument = fopPlugin.mathML2SVG(mathmlDocument)
    val sizes: Sizes = Sizes(svgDocument)

    new ImageGraphics2D(src.getInfo, new Graphics2DImagePainter {
      override def getImageSize: Dimension = sizes.getDimension

      override def paint(graphics2d: Graphics2D, rectangle2d: Rectangle2D): Unit = {
        //val x: Float = 0
        //val y: Float = sizes.ascent
        val  hints: RenderingHints = graphics2d.getRenderingHints
        hints.add(new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON))
        hints.add(new RenderingHints(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE))
        hints.add(new RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY))
        graphics2d.setRenderingHints(hints)

        // see org.apache.fop.image.loader.batik.ImageConverterSVG2G2D for conversion code - but:
        // "Specialized renderers may want to provide specialized adapters to profit
        // from target-format features (for example with PDF or PS)."
      }
    })
  }
}
