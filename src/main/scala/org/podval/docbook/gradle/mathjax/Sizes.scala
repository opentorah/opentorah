package org.podval.docbook.gradle.mathjax

import java.awt.Dimension
import java.awt.geom.Point2D

import org.apache.fop.datatypes.Length
import org.apache.fop.fo.properties.FixedLength
import org.apache.xmlgraphics.image.loader.ImageSize
import org.apache.xmlgraphics.util.UnitConv
import org.w3c.dom.svg.SVGDocument

final class Sizes private(
  viewPortWidth: Float,
  viewBoxWidth: Float,
  viewPortHeight: Float,
  viewBoxHeight: Float,
  viewBoxMinX: Float,
  verticalAlign: Float,
  viewBoxMinY: Float,
  fontSize: Float
) {
  def width: Float = viewPortWidth * xScale

  def height: Float = viewPortHeight * yScale

  def descent: Float = - verticalAlign * yScale // - viewBoxMinY * weirdToPoints * yScale

  def xScale: Float = viewBoxWidth / viewPortWidth * weirdToPoints

  def yScale: Float = viewBoxHeight / viewPortHeight * weirdToPoints

  def weirdToPoints: Float = fontSize / Sizes.Points2Millipoints

  override def toString: String =
    s"Sizes(vpWidth=$viewPortWidth; vbWidth=$viewBoxWidth; xScale=$xScale; width=$width;" +
    s"   vpHeight=$viewPortHeight; vbHeight=$viewBoxHeight; yScale=$yScale; height=$height" +
    s"   minX=$viewBoxMinX; fontSize=$fontSize; verticalAlign=$verticalAlign; minY=$viewBoxMinY; descent=$descent)"

  import Sizes.toMilliPoints

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
    val fontSize: Float = FontSizeAttribute.doGet(svgDocument)

    def exsToPixels(value: Float): Float = value * fontSize * 0.5f // assuming source resolution 1:1, 1 point = 1 pixel
    def pixels(name: String): Float = exsToPixels(exs(svgDocument.getRootElement.getAttribute(name)))

    val viewBoxStr: String = svgDocument.getRootElement.getAttribute("viewBox")
    val viewBox: Array[Float] = viewBoxStr.split(" ").map(_.toFloat)

    val result = new Sizes(
      viewPortWidth = pixels("width"),
      viewBoxWidth = viewBox(2),
      viewPortHeight = pixels("height"),
      viewBoxHeight = viewBox(3),
      viewBoxMinX = viewBox(0),
      verticalAlign = exsToPixels(getVerticalAlign(svgDocument)),
      viewBoxMinY = viewBox(1),
      fontSize = fontSize
    )

    result
  }

  private val verticalAlignCss: String = "vertical-align:"
  private val ex: String = "ex"
  private def getVerticalAlign(svgDocument: SVGDocument): Float =
    svgDocument.getRootElement.getAttribute("style")
      .split(";").map(_.trim).filterNot(_.isEmpty)
      .find(a => a.startsWith(verticalAlignCss))
      .map(_.drop(verticalAlignCss.length))
      .map(exs)
      .getOrElse(0)

  private def exs(value: String): Float = {
    require(value.endsWith(ex))
    value.dropRight(ex.length).trim.toFloat
  }

  private def toMilliPoints(value: Float): Int = Math.round(value * Points2Millipoints)

  val Points2Millipoints: Float = 1000.0f
}
