package org.opentorah.mathjax

import java.awt.Dimension
import java.awt.geom.Point2D
import org.apache.fop.datatypes.Length
import org.apache.fop.fo.properties.FixedLength
import org.apache.xmlgraphics.image.loader.ImageSize
import org.opentorah.xml.{Attribute, Namespace}
import org.w3c.dom.svg.SVGDocument

/* Note:
 Reading of the code that creates SVG and sets its sizes
 (https://github.com/mathjax/MathJax/blob/master/unpacked/jax/output/SVG/jax.js)
 made clear that:
 - viewBox sizes are in milli-ems, and thus can be converted to millipoints by scaling by the fontSize
   (MathJax internally assumes em to be 10 points);
 - viewbox minY is negative SVG height, and viewBox height is SVG height + SVG depth,
   so depth (descent) can be calculated as viewbox height + viewbox minY;
 - vertical-align (in exs) in the style attribute is not depth, so I don't need to use it;
 - viewport sizes (in exs) are calculated from viewbox sizes, so I don't need to use them;
 - MathJax assumes ex height of 430.554 milli-ems (WTF?!), while Batik assumes ex height of 500 milli-ems,
   so before handing the SVG image to Batik, I need to convert viewport sizes to units that are interpreted
   the same way by MathJax and Batik: points (see setViewPortSizes()).
*/
final class Sizes private(
  fontSize: Float,       // in points
  width: Float,          // in milli-ems
  height: Float,         // in milli-ems
  minX: Float,           // in milli-ems
  minY: Float            // in milli-ems
) {
  def depth: Float = height + minY

  override def toString: String =
    s"Sizes(fontSize = $fontSize; minX=$minX; width=$width; minY=$minY; height=$height; depth=$depth)"

  private def toMilliPoints(value: Float): Int = Math.round(value * fontSize)

  private def toPoints(value: Float): Float = value * fontSize / Sizes.points2Millipoints

  def getPoint: Point2D = new Point2D.Float(
    toPoints(width),
    toPoints(height)
  )

  def getIntrinsicAlignmentAdjust: Length =
    FixedLength.getInstance(-toPoints(depth), "pt")

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

  def setViewPortSizes(svgDocument: SVGDocument): Unit = {
    def set(name: String, value: Float): Unit =
      svgDocument.getRootElement.setAttribute(name, toPoints(value).toString + "pt")
    set("width", width)
    set("height", height)
  }
}

object Sizes {
  val inches2points: Int = 72

  val points2Millipoints: Float = 1000.0f

  val mathJaxExInEms: Float = 0.430554f

  val batikExInEms: Float = 0.5f

  def apply(svgDocument: SVGDocument): Sizes = {
    val viewBox: Array[Float] = svgDocument.getRootElement
      .getAttribute("viewBox")
      .split(" ")
      .map(_.toFloat)

    new Sizes(
      fontSize = FontSizeAttribute.doGet(svgDocument),
      minX = viewBox(0),
      minY = viewBox(1),
      width = viewBox(2),
      height = viewBox(3)
    )
  }

  /**
    * Font size (in points) used for the output.
    */
  @SerialVersionUID(1L)
  case object FontSizeAttribute extends Attribute.FloatAttribute("fontSize") {
    override def namespace: Option[Namespace] = Some(MathJax.Namespace)
    override def default: Float = 12.0f
  }
}
