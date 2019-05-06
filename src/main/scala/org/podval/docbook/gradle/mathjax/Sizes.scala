package org.podval.docbook.gradle.mathjax

import java.awt.Dimension
import java.awt.geom.Point2D

import org.apache.fop.datatypes.Length
import org.apache.fop.fo.properties.FixedLength
import org.apache.xmlgraphics.image.loader.ImageSize
import org.apache.xmlgraphics.util.UnitConv
import org.w3c.dom.svg.SVGDocument

final class Sizes(width: Float, ascent: Float, descent: Float) {
  import Sizes.toMilliPoints

  private def height: Float = ascent + descent

  def getPoint: Point2D = new Point2D.Float(width, height)

  def getIntrinsicAlignmentAdjust: Length = FixedLength.getInstance(-descent, "pt")

  // NOTE: this is the only method that scales the values;
  // other callers do not have access to sourceResolution.
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
    val descent: Float = getDescent(svgDocument)
    new Sizes(
      width = svgDocument.getRootElement.getWidth.getBaseVal.getValue,
      ascent = svgDocument.getRootElement.getHeight.getBaseVal.getValue - descent,
      descent = descent
    )
  }

  private val verticalAlignCss: String = "vertical-align:"
  private val verticalAlignUnits: String = "ex" // TODO can it be in other units and require proper parsing?
  private def getDescent(svgDocument: SVGDocument): Float =
    svgDocument.getRootElement.getAttribute("style")
      .split(";").map(_.trim).filterNot(_.isEmpty)
      .find(a => a.startsWith(verticalAlignCss) && a.endsWith("ex"))
      .map(_.drop(verticalAlignCss.length).dropRight(verticalAlignUnits.length).trim)
      .map(a => -a.toFloat)
      .getOrElse(0.0f)

  private def toMilliPoints(value: Float): Int = Math.round(value * Points2Millipoints) // TODO .ceil?

  val Points2Millipoints: Float = 1000.0f
}
