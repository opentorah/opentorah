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

/*
  Inspired by the JEuclid FOP plugin.

  TODO PreloaderMathML typesets MathML to SVG and hands it over to FOP for rendering;

  - it should be possible to typeset right in the MathJaxElement.processNode() to
  avoid duplicate conversions;

  - get clarity on the scaling factor that PreloaderMathML does have access to,
  but MathJaxElement doesn't;

  - find a way to configure SVG rendering with hints and such
  (see ImageLoader, ImageConverterMathML2G2D);

  - the same is true for SVG rendering in xmlHandler - if I need that at all?

  - JEuclid's MathML preloader also parses MathML from a stream. Do I need this?

  - parameters not relevant to MathJax should be removed, and the ones relevant - set properly :)

  - use Scala's Serializable for Parameters - and set  'private static final long serialVersionUID = 1L'

  - deal with nullable Parameters;

  - add MathJax configuration (fonts, extensions etc.);

  - handle re-configuration of MathJax for fonts used in FO;


  If we need to make the pipeline longer:
  - in PreloaderMathML, set imageInfo.mimeType to FopPlugin.MathMLMimeType;
  - in PreloaderMathMl, set image.rootNameSpace to FopPlugin.MathMLNameSpace;
  - complete ImageConverterMathML2G2D;
  - configure imageLoaderFactory and imageConverter
 */
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

  val MathMLElementName: String = "math"

  val MathMLMimeType: String = "application/mathml+xml"

  val Points2Millipoints: Float = 1000.0f

  def configure(fopFactory: FopFactory, nodeModulesRoot: File): Unit =
    new FopPlugin(nodeModulesRoot).configure(fopFactory)

  def isMathML(document: Document): Boolean = {
    val rootNode: Element = document.getDocumentElement
    (rootNode.getNamespaceURI == FopPlugin.MathMLNameSpace) &&
      (rootNode.getNodeName == FopPlugin.MathMLElementName)
  }

  def mathML2SVG(mathMLDocument: Document, mathJax: MathJax): SVGDocument = {
    val parameters: Parameters = Parameters(mathMLDocument.getDocumentElement)

    val mathml: String = toString(mathMLDocument)
    val svg: String = mathJax.typeset2String(mathml, MathJax.MathML, MathJax.Svg)

    val in = new UnclosableInputStream(new StringBufferInputStream(svg))
    val length: Int = in.available()
    in.mark(length + 1)
    val svgDocument: SVGDocument = svgFactory.createSVGDocument(null, in)

    // transplant the parameters
    parameters.serializeInto(svgDocument.getRootElement)

    // supply (just) the font size for the sizes calculations
    val mathSize: Float = parameters.getParameter(Parameters.MathSize)
    svgDocument.getRootElement.asInstanceOf[SVGOMElement].setSVGContext(new SVGContext {
      override def getFontSize: Float = mathSize

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

    svgDocument
  }

  def toString(document: Document): String = xmlSerializer.writeToString(document)

  private val xmlSerializer: LSSerializerImpl = new org.apache.xml.serializer.dom3.LSSerializerImpl

  private val svgFactory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(PreloaderSVG.getParserName)

  final class Sizes(val width: Float, val ascent: Float, val descent: Float) {
    def height: Float = ascent + descent
  }

  def getSizes(svgDocument: SVGDocument): Sizes = {
    val descent: Float = getDescent(svgDocument)
    val width = svgDocument.getRootElement.getWidth.getBaseVal.getValue
    val height = svgDocument.getRootElement.getHeight.getBaseVal.getValue
    new Sizes(
      width = width,
      ascent = height - descent,
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

  def toMilliPoints(value: Float): Int = Math.round(value * FopPlugin.Points2Millipoints) // TODO .ceil?
}
