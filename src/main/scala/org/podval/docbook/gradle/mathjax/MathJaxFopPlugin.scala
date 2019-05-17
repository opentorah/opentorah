package org.podval.docbook.gradle.mathjax

import java.io.{File, InputStream, StringBufferInputStream}

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.fop.apps.FopFactory
import org.apache.fop.fo.{ElementMapping, FONode}
import org.apache.fop.image.loader.batik.PreloaderSVG
import org.apache.fop.render.{Renderer, RendererContext, XMLHandler}
import org.apache.fop.util.UnclosableInputStream
import org.apache.xmlgraphics.image.loader.{Image, ImageFlavor, ImageInfo, ImageSessionContext}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImageLoader, AbstractImageLoaderFactory, ImageXMLDOM}
import org.apache.xmlgraphics.image.loader.spi.{ImageImplRegistry, ImageLoader, ImageLoaderFactory}
import org.podval.docbook.gradle.Xml
import org.podval.docbook.gradle.Namespace.MathML
import org.w3c.dom.{DOMImplementation, Document}
import org.w3c.dom.svg.SVGDocument

//  Inspired by the JEuclid FOP plugin.
final class MathJaxFopPlugin(nodeModulesRoot: File, mathJaxConfiguration: MathJax.Config = MathJax.Config()) {

  def typeset(mathMLDocument: Document): SVGDocument = {
    val mode: String = Parameter.Mode.get(mathMLDocument).getOrElse(MathJax.MathML.input)
    val isInline: Boolean = Parameter.Display.get(mathMLDocument).getOrElse(false)

    val input: MathJax.Input = mode match {
      case MathJax.Tex.input => if (isInline) MathJax.TexInline else MathJax.Tex
      case MathJax.AsciiMath.input => MathJax.AsciiMath
      case _ => MathJax.MathML
    }

    typeset(
      what = if (input == MathJax.MathML) Xml.toString(mathMLDocument) else MathReader.unwrap(mathMLDocument),
      input = input,
      fontSize = Parameter.FontSize.get(mathMLDocument).get
    )
  }

  def typeset(what: String, input: MathJax.Input, fontSize: Float): SVGDocument = {
    val svg: String = typeset(what, input, fontSize.toInt, MathJax.Svg)

    val in: InputStream = new UnclosableInputStream(new StringBufferInputStream(svg))
    val length: Int = in.available()
    in.mark(length + 1)
    val result: SVGDocument = MathJaxFopPlugin.svgFactory.createSVGDocument(null, in)

    // set font size on the resulting SVG - it is needed for the sizes calculations:
    Parameter.FontSize.set(fontSize, result)

    result
  }

  def typeset(what: String, input: MathJax.Input, fontSize: Float, output: MathJax.Output): String = {
    // NOTE: some tests failed unless I typeset specific TeX math first; some - even then;
    // re-configuring and forcibly re-starting MathJax before each typeset call breaks the tests even more;
    // sometimes, stopping Gradle daemon helped; once, JVM crashed; once, I got:
    //   Invalid V8 thread access: current thread is Thread[Execution worker for ':',5,main]
    //   while the locker has thread Thread[Execution worker for ':',5,]
    // Conclusion: MathJax has to be created, used and disposed by the same thread - duh!
    // For now, I just create, use and dispose a fresh MathJax instance for every typesetting -
    // but should probably do the worker thing mentioned in the J2V8 documentation.
    //
    // Some tests fail if their order is reversed when mathJax instance is re-used;
    // this doesn't look like a threading issue - or maybe whatever I "solved" by using fresh MathJax instance
    // for each typesetting wasn't (just) a threading issue either?

    val mathJax: MathJax = new MathJax(nodeModulesRoot)
    mathJax.configure(mathJaxConfiguration)

    val result: String = mathJax.typeset2String(what, input, output, fontSize.toInt)

    mathJax.close()

    result
  }

  private def configure(fopFactory: FopFactory): Unit = {
    fopFactory.getElementMappingRegistry.addElementMapping(elementMapping)
    fopFactory.getXMLHandlerRegistry.addXMLHandler(xmlHandler(this))

    val images: ImageImplRegistry = fopFactory.getImageManager.getRegistry

    images.registerPreloader(new PreloaderMathML(this))

    //images.registerLoaderFactory(imageLoaderFactory)
    //images.registerConverter(new ImageConverterMathML2G2D(this))
  }

  private def elementMapping: ElementMapping = new ElementMapping {
    namespaceURI = MathML.uri

    override def getDOMImplementation: DOMImplementation = ElementMapping.getDefaultDOMImplementation

    override protected def initialize(): Unit = {
      if (foObjs == null) {
        foObjs = new java.util.HashMap

        foObjs.put(MathML.math, new ElementMapping.Maker {
          override def make(parent: FONode): FONode = new MathJaxElement(parent, MathJaxFopPlugin.this)
        })
        foObjs.put(ElementMapping.DEFAULT, new ElementMapping.Maker {
          override def make(parent: FONode): FONode = new MathJaxObj(parent)
        })
      }
    }
  }

  private def xmlHandler(fopPlugin: MathJaxFopPlugin): XMLHandler = new XMLHandler() {

    override def getNamespace: String = MathML.uri

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
      fopPlugin.typeset(document),
      ns
    )
  }

  private def imageLoaderFactory: ImageLoaderFactory = new AbstractImageLoaderFactory {
    override def getSupportedMIMETypes: Array[String] = Array(MathML.mimeType)
    override def getSupportedFlavors(mime: String): Array[ImageFlavor] = Array(ImageFlavor.XML_DOM)
    override def getUsagePenalty(mime: String, flavor: ImageFlavor): Int = 0
    override def isAvailable: Boolean = true

    override def newImageLoader(targetFlavor: ImageFlavor): ImageLoader = {
      require(targetFlavor == ImageFlavor.XML_DOM)

      new AbstractImageLoader {
        override def getTargetFlavor: ImageFlavor = targetFlavor

        override def loadImage(info: ImageInfo, hints: java.util.Map[_, _], session: ImageSessionContext): Image = {
          require(info.getMimeType == MathML.mimeType)
          val result: ImageXMLDOM = info.getOriginalImage.asInstanceOf[ImageXMLDOM]
          require(MathML.is(result.getRootNamespace))
          result
        }
      }
    }
  }
}

object MathJaxFopPlugin {

  def configure(fopFactory: FopFactory, nodeModulesRoot: File, mathJaxConfiguration: MathJax.Config): Unit =
    new MathJaxFopPlugin(nodeModulesRoot, mathJaxConfiguration)
      .configure(fopFactory)

  private val svgFactory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(PreloaderSVG.getParserName)
}
