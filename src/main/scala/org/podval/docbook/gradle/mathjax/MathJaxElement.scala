package org.podval.docbook.gradle.mathjax

import java.awt.geom.Point2D

import org.apache.fop.datatypes.Length
import org.apache.fop.fo.{FOEventHandler, FONode, PropertyList}
import org.xml.sax.{Attributes, Locator}

class MathJaxElement(parent: FONode, fopPlugin: MathJaxFopPlugin) extends MathJaxObj(parent) {

  private var parameters: Option[Parameters] = None

  override protected def createPropertyList(
    pList: PropertyList,
    foEventHandler: FOEventHandler
  ): PropertyList = {
    parameters = Some(new Parameters)

    val commonFont = pList.getFontProps

    parameters.get.setParameter(Parameters.FontSize,
      (commonFont.fontSize.getNumericValue / Sizes.Points2Millipoints).toFloat)

    parameters.get.setParameter(Parameters.Fonts,
      commonFont.getFontState(getFOEventHandler.getFontInfo).toList.map(_.getName))

    super.createPropertyList(pList, foEventHandler)
  }

  override def processNode(
    elementName: String,
    locator: Locator,
    attlist: Attributes,
    propertyList: PropertyList
  ): Unit = {
    super.processNode(elementName, locator, attlist, propertyList)
    parameters.foreach(_.serializeInto(createBasicDocument().getDocumentElement))
  }

  // NOTE: It is tempting to typeset MathML to SVG right here to avoid duplicate conversions
  // - one here in getSizes() and another one in PreloaderMathML -
  // but resulting SVG is then preloaded by FOP itself (our preloader doesn't get called),
  // and since there is no CSSEngine, there is no font size, which crashes in sizes calculations.
  //
  // Namespace of the document element is not modifiable, so I can't just re-label SVG as MathJax
  // to force FOP to call our preloader. I guess the only way is to wrap resulting SVG in a document
  // in the MathJax namespace...
  //
  //   override def finalizeNode(): Unit = {
  //     doc = MathJaxFopPlugin.mathML2SVG(doc, mathJax)
  //   }

  private var sizes: Option[Sizes] = None

  private def getSizes: Sizes = sizes.getOrElse {
    val result = Sizes(fopPlugin.mathML2SVG(doc))
    sizes = Some(result)
    result
  }

  override def getDimension(view: Point2D): Point2D = getSizes.getPoint

  override def getIntrinsicAlignmentAdjust: Length = getSizes.getIntrinsicAlignmentAdjust
}
