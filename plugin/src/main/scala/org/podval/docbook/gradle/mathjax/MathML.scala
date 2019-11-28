package org.podval.docbook.gradle.mathjax

import java.awt.geom.Point2D

import org.apache.fop.datatypes.Length
import org.apache.fop.fo.{FOEventHandler, FONode, PropertyList, XMLObj}
import org.podval.fop.mathjax.Sizes
import org.podval.fop.xml.{Attribute, AttributeInfo}
import org.w3c.dom.{Document, Element}
import org.xml.sax.{Attributes, Locator}

final class MathML(parent: FONode, fopPlugin: MathJaxFopPlugin) extends MathML.Obj(parent) {

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
  //     doc = fopPlugin.typeset(doc)
  //   }

  private var sizes: Option[Sizes] = None

  private def getSizes: Sizes = sizes.getOrElse {
    val result = Sizes(fopPlugin.typeset(doc))
    sizes = Some(result)
    result
  }

  override def getDimension(view: Point2D): Point2D = getSizes.getPoint

  override def getIntrinsicAlignmentAdjust: Length = getSizes.getIntrinsicAlignmentAdjust
}

object MathML {

  // Note: only MathMLObj.getNormalNamespacePrefix() needs the prefix;
  // everywhere else default mapping is assumed.
  object Namespace extends org.podval.fop.xml.Namespace(
    uri = "http://www.w3.org/1998/Math/MathML",
    prefix = "mathml"
  )

  /**
    * Display mode: inline or block (display math).
    */
  @SerialVersionUID(1L)
  case object DisplayAttribute extends Attribute[Boolean] {
    private val inline: String = "inline"
    private val block: String = "block"
    private val values: Set[String] = Set(inline, block)

    override def namespace: org.podval.fop.xml.Namespace = Namespace
    override def name: String = "display"

    override def fromString(value: String): Boolean = {
      require(values.contains(value))
      value == inline
    }

    override def toString(value: Boolean): String = if (value) inline else block

    def default: Boolean = false
  }

  class Obj(parent: FONode) extends XMLObj(parent) {

    override def getNamespaceURI: String = Namespace.uri

    override def getNormalNamespacePrefix: String = Namespace.prefix
  }

  val mimeType: String = "application/mathml+xml"

  val math: String = "math"
  val mrow: String = "mrow"
  val mi: String = "mi"

  def unwrap(mathMLDocument: Document): String = mathMLDocument.getDocumentElement
    .getElementsByTagName(MathML.mrow).item(0).asInstanceOf[Element]
    .getElementsByTagName(MathML.mi).item(0).asInstanceOf[Element]
    .getTextContent
}
