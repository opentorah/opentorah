package org.podval.docbook.gradle.mathjax

import java.awt.geom.{AffineTransform, Rectangle2D}
import java.io.StringBufferInputStream
import java.io.File

import org.apache.batik.anim.dom.{SAXSVGDocumentFactory, SVGOMElement}
import org.apache.batik.dom.svg.SVGContext
import org.apache.fop.apps.FopFactory
import org.apache.fop.fo.{ElementMapping, FONode}
import org.apache.fop.image.loader.batik.PreloaderSVG
import org.apache.fop.render.{Renderer, RendererContext, XMLHandler}
import org.apache.fop.util.UnclosableInputStream
import org.apache.xml.serializer.dom3.LSSerializerImpl
import org.apache.xmlgraphics.image.loader.{Image, ImageFlavor, ImageInfo, ImageSessionContext}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImageLoader, AbstractImageLoaderFactory, ImageXMLDOM}
import org.apache.xmlgraphics.image.loader.spi.{ImageImplRegistry, ImageLoader, ImageLoaderFactory}
import org.w3c.dom.{DOMImplementation, Document, Element}
import org.w3c.dom.svg.SVGDocument

//  Inspired by the JEuclid FOP plugin.
final class FopPlugin(nodeModulesRoot: File) {

  private def configure(fopFactory: FopFactory): Unit = {
    fopFactory.getElementMappingRegistry.addElementMapping(elementMapping)
    fopFactory.getXMLHandlerRegistry.addXMLHandler(xmlHandler)

    val images: ImageImplRegistry = fopFactory.getImageManager.getRegistry

    images.registerPreloader(new PreloaderMathML(getMathJax))

    //images.registerLoaderFactory(imageLoaderFactory)
    //images.registerConverter(new ImageConverterMathML2G2D(getMathJax))
  }

  private def elementMapping: ElementMapping = new ElementMapping {
    namespaceURI = FopPlugin.MathMLNameSpace

    override def getDOMImplementation: DOMImplementation = ElementMapping.getDefaultDOMImplementation

    override protected def initialize(): Unit = {
      if (foObjs == null) {
        foObjs = new java.util.HashMap

        foObjs.put(FopPlugin.MathMLElementName, new ElementMapping.Maker {
          override def make(parent: FONode): FONode = new MathJaxElement(parent, getMathJax)
        })
        foObjs.put(ElementMapping.DEFAULT, new ElementMapping.Maker {
          override def make(parent: FONode): FONode = new MathJaxObj(parent)
        })
      }
    }
  }

  private def xmlHandler: XMLHandler = new XMLHandler {

    override def getNamespace: String = FopPlugin.MathMLNameSpace

    override def supportsRenderer(renderer: Renderer): Boolean = renderer.getGraphics2DAdapter != null

    // From XMLHandler.handleXML() documentation:
    //   The implementation may convert the XML document internally to another
    //   XML dialect (SVG, for example) and call renderXML() on the AbstractRenderer
    //   again (which can be retrieved through the RendererContext).
    override def handleXML(
      rendererContext: RendererContext,
      document: Document,
      ns: String
    ): Unit = rendererContext.getRenderer.renderXML(
      rendererContext,
      FopPlugin.mathML2SVG(document, getMathJax),
      ns
    )
  }

  private def imageLoaderFactory: ImageLoaderFactory = new AbstractImageLoaderFactory {
    override def getSupportedMIMETypes: Array[String] = Array(FopPlugin.MathMLMimeType)
    override def getSupportedFlavors(mime: String): Array[ImageFlavor] = Array(ImageFlavor.XML_DOM)
    override def getUsagePenalty(mime: String, flavor: ImageFlavor): Int = 0
    override def isAvailable: Boolean = true

    override def newImageLoader(targetFlavor: ImageFlavor): ImageLoader = {
      require(targetFlavor == ImageFlavor.XML_DOM)

      new AbstractImageLoader {
        override def getTargetFlavor: ImageFlavor = targetFlavor

        override def loadImage(info: ImageInfo, hints: java.util.Map[_, _], session: ImageSessionContext): Image = {
          require(info.getMimeType == FopPlugin.MathMLMimeType)
          val result: ImageXMLDOM = info.getOriginalImage.asInstanceOf[ImageXMLDOM]
          require(result.getRootNamespace == FopPlugin.MathMLNameSpace)
          result
        }
      }
    }
  }

  private var mathJax: Option[MathJax] = None

  private def getMathJax: MathJax = mathJax.getOrElse {
    val result = new MathJax(nodeModulesRoot)
    result.open()
    mathJax = Some(result)
    result
  }
}

object FopPlugin {
  val MathMLNameSpace: String = "http://www.w3.org/1998/Math/MathML"

  val MathMLNameSpacePrefix: String = "mathml"

  val MathMLElementName: String = "math"

  val MathMLMimeType: String = "application/mathml+xml"

  val MathJaxNameSpace = "http://podval.org/mathjax/ns/ext"

  val MathJaxAttributePrefix: String = "mathjax:"

  def configure(fopFactory: FopFactory, nodeModulesRoot: File): Unit =
    new FopPlugin(nodeModulesRoot).configure(fopFactory)

  def mathML2SVG(mathMLDocument: Document, mathJax: MathJax): SVGDocument = {
    val parameters: Parameters = Parameters(mathMLDocument.getDocumentElement)

    val svg: String = {
      // TODO inline or display: look at the attribute(s) of the foreign object element...
      val mode = mathMLDocument.getDocumentElement.getAttribute("mode")
      if (mode == "tex") {
        val mrow = mathMLDocument.getDocumentElement.getElementsByTagName("mrow").item(0).asInstanceOf[Element]
        val mi = mrow.getElementsByTagName("mi").item(0).asInstanceOf[Element]
        val tex = mi.getTextContent
        mathJax.typeset2String(tex, MathJax.Tex, MathJax.Svg)
      } else {
        val mathml: String = toString(mathMLDocument)
        mathJax.typeset2String(mathml, MathJax.MathML, MathJax.Svg)
      }
    }

    val in = new UnclosableInputStream(new StringBufferInputStream(svg))
    val length: Int = in.available()
    in.mark(length + 1)
    val svgDocument: SVGDocument = svgFactory.createSVGDocument(null, in)

    // transplant the parameters
    parameters.serializeInto(svgDocument.getRootElement)

    toSVG(svgDocument)
  }

  def toSVG(document: Document): SVGDocument = {
    val result = document.asInstanceOf[SVGDocument]

    val parameters: Parameters = Parameters(result.getDocumentElement)

    // supply (just) the font size for the sizes calculations
    val fontSize: Float = parameters.getParameter(Parameters.FontSize)

    result.getDocumentElement.asInstanceOf[SVGOMElement].setSVGContext(new SVGContext {
      override def getFontSize: Float = fontSize

      override def getPixelUnitToMillimeter: Float = ???
      override def getPixelToMM: Float = ???
      override def getBBox: Rectangle2D = ???
      override def getScreenTransform: AffineTransform = ???
      override def setScreenTransform(at: AffineTransform): Unit = ???
      override def getCTM: AffineTransform = ???
      override def getGlobalTransform: AffineTransform = ???
      override def getViewportWidth: Float = ???
      override def getViewportHeight: Float = ???
    })

    result
  }

  def toString(document: Document): String = xmlSerializer.writeToString(document)

  private val xmlSerializer: LSSerializerImpl = new org.apache.xml.serializer.dom3.LSSerializerImpl

  private val svgFactory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(PreloaderSVG.getParserName)
}
