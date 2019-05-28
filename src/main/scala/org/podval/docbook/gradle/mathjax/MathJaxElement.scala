package org.podval.docbook.gradle.mathjax

import java.awt.geom.Point2D

import org.apache.fop.datatypes.Length
import org.apache.fop.fo.{FOEventHandler, FONode, PropertyList}
import org.podval.docbook.gradle.xml.{AttributeInfo, Namespace}
import org.xml.sax.helpers.AttributesImpl
import org.xml.sax.{Attributes, Locator}

class MathJaxElement(parent: FONode, fopPlugin: MathJaxFopPlugin) extends MathJaxObj(parent) {

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
    super.processNode(elementName, locator, sortAttributes(attlist), propertyList)

    createBasicDocument()

    FontSizeAttribute.set(fontSize.get, getDOMDocument)
  }

  // Note: XMLObj.setAttributes() sets namespace on an attribute only if it already saw
  // the declarations of that namespace, so I am making sure that they are there (and in the beginning);
  // even then, XMLObj.setAttributes() sets unprefixed qualifield name for namespaced attributes -
  // but somehow they are detected correctly in MathJax.typeset()...
  private def sortAttributes(attlist: Attributes): Attributes = {
    val attributes: Seq[AttributeInfo] = AttributeInfo(attlist)
    val nonXmlnsAttributes: Seq[AttributeInfo] = attributes.filterNot(_.isXmlns)
    val usedNamespaces: Set[Namespace] = nonXmlnsAttributes.flatMap(_.namespace).toSet
    val declaredNamespaces: Set[Namespace] = attributes.flatMap(_.declaredNamespace).toSet

    val result = new AttributesImpl

    for (namespace <- usedNamespaces -- declaredNamespaces) namespace.declare(result)
    for (attribute <- nonXmlnsAttributes) attribute.addTo(result)

    result
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
  //     doc = MathJaxFopPlugin.mathML2SVG(doc, mathJax)
  //   }

  private var sizes: Option[Sizes] = None

  private def getSizes: Sizes = sizes.getOrElse {
    val result = Sizes(fopPlugin.withMathJax(_.typeset(doc)))
    sizes = Some(result)
    result
  }

  override def getDimension(view: Point2D): Point2D = getSizes.getPoint

  override def getIntrinsicAlignmentAdjust: Length = getSizes.getIntrinsicAlignmentAdjust
}
