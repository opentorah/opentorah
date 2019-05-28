package org.podval.docbook.gradle.mathjax

import java.awt.Dimension
import java.awt.geom.Point2D

import org.apache.fop.datatypes.Length
import org.apache.fop.fo.properties.FixedLength
import org.apache.xmlgraphics.image.loader.ImageSize
import org.w3c.dom.svg.SVGDocument

// From the code that creates SVG and sets its sizes
// (https://github.com/mathjax/MathJax/blob/master/unpacked/jax/output/SVG/jax.js)
// it became clear that:
// - viewBox sizes are in milli-ems;
// - ex height is Sizes.xHeight milli-ems, so scaling from millipoints to exs is: / Sizes.xHeight;
// - minY is -height;
// - viewBox height is height+depth;
// - scaling for the fontSize is: * fontSize (MathJax internally assumes em to be 10 points).
final class Sizes private(
  width: Float,          // in milli-ems
  viewPortWidth: Float,  // in exs; = width / Sizes.xHeight
  height: Float,         // in milli-ems
  viewPortHeight: Float, // = height / Sizes.xHeight
  minX: Float,           // in milli-ems
  minY: Float,           // in milli-ems
  verticalAlign: Float,  // in exs
  fontSize: Float        // in points
) {
  def depth: Float = height + minY

  private def toPoints(value: Float): Float = value * fontSize / Sizes.points2Millipoints

  def widthInPoints: Float = toPoints(width)

  def heightInPoints: Float = toPoints(height)

  def depthInPoints: Float = toPoints(depth)

  private def toMilliPoints(value: Float): Int = Math.round(value * fontSize)

  override def toString: String =
    s"Sizes(fontSize = $fontSize; minX=$minX; width=$width; minY=$minY; height=$height; depth=$depth)"

  def getPoint: Point2D = new Point2D.Float(
    widthInPoints,
    heightInPoints
  )

  def getIntrinsicAlignmentAdjust: Length =
    FixedLength.getInstance(-depthInPoints, "pt")

  def getImageSize(sourceResolution: Float): ImageSize = {
    val scale: Float = Sizes.inches2points / sourceResolution
    def millipoints(value: Float): Int = toMilliPoints(value * scale)
    val result: ImageSize = new ImageSize
    result.setSizeInMillipoints(
      millipoints(width),
      millipoints(height)
    )
    result.setBaselinePositionFromBottom(millipoints(depth))
    result.setResolution(sourceResolution)
    result.calcPixelsFromSize()
    result
  }

  def getDimension: Dimension = new Dimension(
    toMilliPoints(width),
    toMilliPoints(height)
  )


  // MathJax and Batik have different x_height assumptions (430.554f and 500.0f respectively, in milli-ems);
  // to avoid mis-scaling, I convert values of the "width" and "height" attributes on the SVG document created
  // by MathJax to points before handing it over to Batik.
  def set(svgDocument: SVGDocument): Unit = {
    val element = svgDocument.getRootElement
    element.setAttribute("width", widthInPoints + "pt")
    element.setAttribute("height", heightInPoints + "pt")
  }
}

object Sizes {
  // MathJax's value for the ex height; WTF is it?
  val xHeight: Float = 430.554f

  val inches2points: Int  = 72

  val points2Millipoints: Float = 1000.0f

  def apply(svgDocument: SVGDocument): Sizes = {
    val fontSize: Float = FontSizeAttribute.doGet(svgDocument)

    // Note: batik assumes x size of 0.5 fontSize, so, assuming source resolution 1point = 1pixel, *fontSize * 0.5f -
    // but MathJax does not :)
    def pixels(name: String): Float = exs(svgDocument.getRootElement.getAttribute(name))

    val viewBoxStr: String = svgDocument.getRootElement.getAttribute("viewBox")
    val viewBox: Array[Float] = viewBoxStr.split(" ").map(_.toFloat)

    val result = new Sizes(
      minX = viewBox(0),
      minY = viewBox(1),
      width = viewBox(2),
      height = viewBox(3),
      viewPortWidth = pixels("width"),
      viewPortHeight = pixels("height"),
      verticalAlign = getVerticalAlign(svgDocument),
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
}
