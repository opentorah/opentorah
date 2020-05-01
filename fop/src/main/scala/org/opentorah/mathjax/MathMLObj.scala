package org.opentorah.mathjax

import java.awt.geom.Point2D
import org.apache.fop.datatypes.Length
import org.apache.fop.fo.{FOEventHandler, FONode, PropertyList, XMLObj}
import org.opentorah.xml.AttributeInfo
import org.xml.sax.{Attributes, Locator}

final class MathMLObj(parent: FONode, mathJax: MathJax) extends MathMLObj.Obj(parent) {

  private var fontSize: Option[Float] = None

  override protected def createPropertyList(
    pList: PropertyList,
    foEventHandler: FOEventHandler
  ): PropertyList = {
    val commonFont = pList.getFontProps

    fontSize = Some((commonFont.fontSize.getNumericValue / Sizes.points2Millipoints).toFloat)

    // fonts: commonFont.getFontState(getFOEventHandler.getFontInfo).toList.map(_.getName)

    super.createPropertyList(pList, foEventHandler)
  }

  override def processNode(
    elementName: String,
    locator: Locator,
    attlist: Attributes,
    propertyList: PropertyList
  ): Unit = {
    super.processNode(elementName, locator, AttributeInfo.sort(attlist), propertyList)

    createBasicDocument()

    Sizes.FontSizeAttribute.set(fontSize.get, getDOMDocument)
  }

  // Note: It is tempting to typeset MathML to SVG right here to avoid duplicate conversions
  // - one here in getSizes() and another one in PreloaderMathML -
  // but resulting SVG is then preloaded by FOP itself (our preloader doesn't get called),
  // and since there is no CSSEngine, there is no font size, which crashes in sizes calculations.
  //
  // Namespace of the document element is not modifiable, so I can't just re-label SVG as MathJax
  // to force FOP to call our preloader. I guess the only way is to wrap resulting SVG in a document
  // in the MathJax namespace...
  //
  //   override def finalizeNode(): Unit = {
  //     doc = mathJax.typeset(doc)
  //   }

  private var sizes: Option[Sizes] = None

  private def getSizes: Sizes = sizes.getOrElse {
    val result = Sizes(mathJax.typeset(doc))
    sizes = Some(result)
    result
  }

  override def getDimension(view: Point2D): Point2D = getSizes.getPoint

  override def getIntrinsicAlignmentAdjust: Length = getSizes.getIntrinsicAlignmentAdjust
}

object MathMLObj {
  class Obj(parent: FONode) extends XMLObj(parent) {

    override def getNamespaceURI: String = MathML.Namespace.uri

    override def getNormalNamespacePrefix: String = MathML.Namespace.prefix
  }
}
