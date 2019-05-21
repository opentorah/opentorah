package org.podval.docbook.gradle.mathjax

import java.awt.Dimension
import java.awt.geom.Point2D

import org.apache.fop.datatypes.Length
import org.apache.fop.fo.properties.FixedLength
import org.apache.xmlgraphics.image.loader.ImageSize
import org.apache.xmlgraphics.util.UnitConv
import org.w3c.dom.svg.SVGDocument

final class Sizes(width: Float, ascent: Float, descent: Float, fontSize: Float) {
  override def toString: String =
    s"Sizes(width=${width}pt, ascent=${ascent}pt, descent=${descent}pt; height=${height}pt; fontSize=${fontSize}pt)"

  import Sizes.toMilliPoints

  private def height: Float = ascent + descent

  def getPoint: Point2D = new Point2D.Float(width, height)

  def getIntrinsicAlignmentAdjust: Length = FixedLength.getInstance(-descent, "pt")

  def getImageSize(sourceResolution: Float): ImageSize = {
    val scale: Float = UnitConv.IN2PT / sourceResolution

    val result: ImageSize = new ImageSize
    result.setSizeInMillipoints(toMilliPoints(width * scale), toMilliPoints(height * scale))
    result.setBaselinePositionFromBottom(toMilliPoints(descent * scale))
    result.setResolution(sourceResolution)
    result.calcPixelsFromSize()
    result
  }

  def getDimension: Dimension = new Dimension(toMilliPoints(width), toMilliPoints(height))
}

object Sizes {
  def apply(svgDocument: SVGDocument): Sizes = {
    val fontSize: Float = FontSizeAttribute.get(svgDocument).get

    def exsToPixels(value: Float): Float = value * fontSize * 0.5f // assuming source resolution 1:1, 1 point = 1 pixel
    def pixels(name: String): Float = exsToPixels(exs(svgDocument.getRootElement.getAttribute(name)))

    val descent: Float = exsToPixels(getDescent(svgDocument))
    new Sizes(
      width = pixels("width"),
      ascent = pixels("height") - descent,
      descent = descent,
      fontSize = fontSize
    )
  }

  private val verticalAlignCss: String = "vertical-align:"
  private val ex: String = "ex"
  private def getDescent(svgDocument: SVGDocument): Float = {
    val value: Option[String] = svgDocument.getRootElement.getAttribute("style")
      .split(";").map(_.trim).filterNot(_.isEmpty)
      .find(a => a.startsWith(verticalAlignCss))
      .map(_.drop(verticalAlignCss.length))

    val result: Float = value.map(exs).getOrElse(0)
    -result
  }

  private def exs(value: String): Float = {
    require(value.endsWith(ex))
    value.dropRight(ex.length).trim.toFloat
  }

  private def toMilliPoints(value: Float): Int = Math.round(value * Points2Millipoints)

  val Points2Millipoints: Float = 1000.0f

  // To calculate width and height using svgDocument.getRootElement.[getWidth | getHeight].getBaseVal.getValue,
  // SVG context that provides fontSize needs to ve set on the document element:
  // Note: Sizes are retrieved without making the calls that need SVG context...
  //  def setContext(svgDocument: SVGDocument, fontSize: Float): Unit = {
  //    svgDocument.getDocumentElement.asInstanceOf[SVGOMElement].setSVGContext(new SVGContext {
  //      override def getFontSize: Float = fontSize
  //
  //      override def getPixelUnitToMillimeter: Float = ???
  //      override def getPixelToMM: Float = ???
  //      override def getBBox: Rectangle2D = ???
  //      override def getScreenTransform: AffineTransform = ???
  //      override def setScreenTransform(at: AffineTransform): Unit = ???
  //      override def getCTM: AffineTransform = ???
  //      override def getGlobalTransform: AffineTransform = ???
  //      override def getViewportWidth: Float = ???
  //      override def getViewportHeight: Float = ???
  //    })
  //  }
}
