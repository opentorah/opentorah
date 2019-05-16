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
import org.apache.xmlgraphics.image.loader.{Image, ImageFlavor, ImageInfo, ImageSessionContext}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImageLoader, AbstractImageLoaderFactory, ImageXMLDOM}
import org.apache.xmlgraphics.image.loader.spi.{ImageImplRegistry, ImageLoader, ImageLoaderFactory}
import org.podval.docbook.gradle.Xml
import org.w3c.dom.{DOMImplementation, Document, Element}
import org.w3c.dom.svg.SVGDocument

//  Inspired by the JEuclid FOP plugin.
final class MathJaxFopPlugin(nodeModulesRoot: File, mathJaxConfiguration: MathJax.Config) {

  def mathML2SVG(mathMLDocument: Document): SVGDocument = {
    // NOTE: some tests failed unless I typeset specific TeX math first; some - even then;
    // re-configuring and forcibly re-starting MathJax before each typeset call breaks the tests even more;
    // sometimes, stopping Gradle daemon helped; once, JVM crashed; once, I got:
    //   Invalid V8 thread access: current thread is Thread[Execution worker for ':',5,main]
    //   while the locker has thread Thread[Execution worker for ':',5,]
    // Conclusion: MathJax has to be created, used and disposed by the same thread - duh!
    // For now, I just create, use and dispose a fresh MathJax instance for every typesetting -
    // but should probably do the worker thing mentioned in the J2V8 documentation.
    val mathJax: MathJax = new MathJax(nodeModulesRoot)
    mathJax.configure(mathJaxConfiguration)
    val result: SVGDocument = MathJaxFopPlugin.mathML2SVG(mathMLDocument, mathJax)
    mathJax.close()
    result
  }

  private def configure(fopFactory: FopFactory): Unit = {
    fopFactory.getElementMappingRegistry.addElementMapping(elementMapping)
    fopFactory.getXMLHandlerRegistry.addXMLHandler(xmlHandler)

    val images: ImageImplRegistry = fopFactory.getImageManager.getRegistry

    images.registerPreloader(new PreloaderMathML(this))

    //images.registerLoaderFactory(imageLoaderFactory)
    //images.registerConverter(new ImageConverterMathML2G2D(this))
  }

  private def elementMapping: ElementMapping = new ElementMapping {
    namespaceURI = MathJaxFopPlugin.MathMLNameSpace

    override def getDOMImplementation: DOMImplementation = ElementMapping.getDefaultDOMImplementation

    override protected def initialize(): Unit = {
      if (foObjs == null) {
        foObjs = new java.util.HashMap

        foObjs.put(MathJaxFopPlugin.MathMLElementName, new ElementMapping.Maker {
          override def make(parent: FONode): FONode = new MathJaxElement(parent, MathJaxFopPlugin.this)
        })
        foObjs.put(ElementMapping.DEFAULT, new ElementMapping.Maker {
          override def make(parent: FONode): FONode = new MathJaxObj(parent)
        })
      }
    }
  }

  private def xmlHandler: XMLHandler = new XMLHandler {

    override def getNamespace: String = MathJaxFopPlugin.MathMLNameSpace

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
      mathML2SVG(document),
      ns
    )
  }

  private def imageLoaderFactory: ImageLoaderFactory = new AbstractImageLoaderFactory {
    override def getSupportedMIMETypes: Array[String] = Array(MathJaxFopPlugin.MathMLMimeType)
    override def getSupportedFlavors(mime: String): Array[ImageFlavor] = Array(ImageFlavor.XML_DOM)
    override def getUsagePenalty(mime: String, flavor: ImageFlavor): Int = 0
    override def isAvailable: Boolean = true

    override def newImageLoader(targetFlavor: ImageFlavor): ImageLoader = {
      require(targetFlavor == ImageFlavor.XML_DOM)

      new AbstractImageLoader {
        override def getTargetFlavor: ImageFlavor = targetFlavor

        override def loadImage(info: ImageInfo, hints: java.util.Map[_, _], session: ImageSessionContext): Image = {
          require(info.getMimeType == MathJaxFopPlugin.MathMLMimeType)
          val result: ImageXMLDOM = info.getOriginalImage.asInstanceOf[ImageXMLDOM]
          require(result.getRootNamespace == MathJaxFopPlugin.MathMLNameSpace)
          result
        }
      }
    }
  }
}

object MathJaxFopPlugin {
  val MathMLNameSpace: String = "http://www.w3.org/1998/Math/MathML"

  val MathMLNameSpacePrefix: String = "mathml"

  val MathMLElementName: String = "math"

  val MathMLMimeType: String = "application/mathml+xml"

  def configure(fopFactory: FopFactory, nodeModulesRoot: File, mathJaxConfiguration: MathJax.Config): Unit =
    new MathJaxFopPlugin(nodeModulesRoot, mathJaxConfiguration).configure(fopFactory)

  private def mathML2SVG(mathMLDocument: Document, mathJax: MathJax): SVGDocument = {
    def unwrap: String = mathMLDocument.getDocumentElement
      .getElementsByTagName ("mrow").item (0).asInstanceOf[Element]
      .getElementsByTagName ("mi").item (0).asInstanceOf[Element]
      .getTextContent

    val parameters: Parameters = Parameters(mathMLDocument)
    val mode: String = parameters.getMode
    val fontSize: Float = parameters.getFontSize
    val ex: Int = parameters.getFontExSize

    val svg: String = mode match {
      case MathJax.Tex.input =>
        mathJax.typeset2String(unwrap, MathJax.Tex, ex = ex)

      case MathJax.AsciiMath.input =>
        mathJax.typeset2String(unwrap, MathJax.AsciiMath, ex = ex)

      case _ =>
        val mathml: String = Xml.toString(mathMLDocument)
        mathJax.typeset2String(mathml, MathJax.MathML, ex = ex)
    }

    val in = new UnclosableInputStream(new StringBufferInputStream(svg))
    val length: Int = in.available()
    in.mark(length + 1)
    val svgDocument: SVGDocument = svgFactory.createSVGDocument(null, in)

    // transplant the parameters
    parameters.serializeInto(svgDocument.getRootElement)

    // supply (just) the font size for the sizes calculations
    svgDocument.getDocumentElement.asInstanceOf[SVGOMElement].setSVGContext(new SVGContext {
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

    svgDocument
  }

  private val svgFactory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(PreloaderSVG.getParserName)
}
