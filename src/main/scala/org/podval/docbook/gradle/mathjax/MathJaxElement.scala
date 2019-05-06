package org.podval.docbook.gradle.mathjax

import java.awt.geom.Point2D

import org.apache.fop.datatypes.Length
import org.apache.fop.fo.{FOEventHandler, FONode, PropertyList}
import org.apache.fop.fo.properties.FixedLength
import org.xml.sax.{Attributes, Locator}

class MathJaxElement(parent: FONode, mathJax: MathJax) extends MathJaxObj(parent) {

  private var parameters: Option[Parameters] = None

  private def setParameter[T](parameter: Parameters.Parameter[T], value: T): Unit = {
    if (parameters.isEmpty) parameters = Some(new Parameters)
    parameters.get.setParameter(parameter, value)
  }

  private var sizes: Option[MathJaxElement.Sizes] = None

  private def getSizes: MathJaxElement.Sizes = sizes.getOrElse {
    val svgDocument = FopPlugin.mathML2SVG(doc, mathJax)
    val viewSizes = FopPlugin.getSizes(svgDocument)
    val result = new MathJaxElement.Sizes(
      size = new Point2D.Float(viewSizes.width, viewSizes.height),
      baseline = FixedLength.getInstance(-viewSizes.descent, "pt")
    )
    sizes = Some(result)
    result
  }

  override def getDimension(view: Point2D): Point2D = getSizes.size

  override def getIntrinsicAlignmentAdjust: Length = getSizes.baseline

  override def processNode(
    elementName: String,
    locator: Locator,
    attlist: Attributes,
    propertyList: PropertyList
  ): Unit = {
    super.processNode(elementName, locator, attlist, propertyList)

    createBasicDocument()

    parameters.foreach(_.serializeInto(getDOMDocument.getDocumentElement))
  }

  override protected def createPropertyList(
    pList: PropertyList,
    foEventHandler: FOEventHandler
  ): PropertyList = {
    val commonFont = pList.getFontProps

    setParameter(Parameters.MathSize,
      (commonFont.fontSize.getNumericValue / FopPlugin.Points2Millipoints).toFloat)

    setParameter(Parameters.FontsSerif,
      commonFont.getFontState(getFOEventHandler.getFontInfo).toList.map(_.getName))

    //    final Property colorProp = pList.get(org.apache.fop.fo.Constants.PR_COLOR)
//    if (colorProp != null) {
//      final Color color = colorProp.getColor(getUserAgent())
//      layoutContext.setParameter(Parameter.MATHCOLOR, color)
//    }
//    final Property bcolorProp = pList.get(org.apache.fop.fo.Constants.PR_BACKGROUND_COLOR)
//    if (bcolorProp != null) {
//      final Color bcolor = bcolorProp.getColor(getUserAgent())
//      layoutContext.setParameter(Parameter.MATHBACKGROUND, bcolor)
//    }

    super.createPropertyList(pList, foEventHandler)
  }
}

object MathJaxElement {
  private final class Sizes(val size: Point2D, val baseline: Length)
}
