package org.podval.docbook.gradle.mathjax

import java.awt.{Dimension, Graphics2D, RenderingHints}
import java.awt.geom.Rectangle2D
import java.io.File

import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import org.apache.fop.fo.{ElementMapping, FONode}
import org.apache.fop.render.{Renderer, RendererContext, XMLHandler}
import org.apache.xmlgraphics.image.loader.{Image, ImageContext, ImageFlavor, ImageInfo, ImageSessionContext}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImageConverter, AbstractImageLoader,
  AbstractImageLoaderFactory, AbstractImagePreloader, ImageGraphics2D, ImageXMLDOM}
import org.apache.xmlgraphics.image.loader.spi.{ImageConverter, ImageLoader, ImageLoaderFactory, ImagePreloader}
import org.apache.xmlgraphics.java2d.Graphics2DImagePainter
import org.podval.docbook.gradle.fop.FopPlugin
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.{DOMImplementation, Document}

//  Inspired by the JEuclid FOP plugin.
final class MathJaxFopPlugin(nodeModulesRoot: File, mathJaxConfiguration: MathJaxConfiguration) extends FopPlugin {

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
  // NOTE: some tests failed unless I typeset specific TeX math first; some - even then;
  def withMathJax[T](f: MathJax => T): T = {
    val mathJax = new MathJax(nodeModulesRoot)
    mathJax.configure(mathJaxConfiguration)

    val result = f(mathJax)

    mathJax.close()

    result
  }

  private def typeset(document: Document): SVGDocument = withMathJax(_.typeset(document))

  override protected def elementMapping: ElementMapping = new ElementMapping {
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

  // Note: Not sure if I need this - it is probably used to read MathML from a file...
  // If I end up using it, make sure SVG rendering works properly...
  override protected def isHandlerNeeded: Boolean = false

  override protected def xmlHandler: XMLHandler = new XMLHandler() {
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
      typeset(document),
      ns
    )
  }

  override protected def imagePreloader: ImagePreloader = new AbstractImagePreloader {
    override def preloadImage(uri: String, src: Source, context: ImageContext): ImageInfo = {
      val document: Document = src.asInstanceOf[DOMSource].getNode.asInstanceOf[Document]

      if (!MathML.is(document)) null else  {
        val svgDocument: SVGDocument = typeset(document)
        val sizes: Sizes = Sizes(svgDocument)
        // convert sizes from exs to points:
        sizes.set(svgDocument)

        val result: ImageInfo = new ImageInfo(uri, Svg.mimeType)
        result.setSize(sizes.getImageSize(context.getSourceResolution))

        // Stash the result to avoid typesetting again:
        result
          .getCustomObjects.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
          .put(ImageInfo.ORIGINAL_IMAGE, new ImageXMLDOM(result, svgDocument, Svg.uri))

        result
      }
    }
  }

  /* Note: PreloaderMathML typesets MathML to SVG and hands it over to FOP for rendering;
     if it turns out to be possible to place/size resulting math correctly in the PDF
     (it is off the baseline, too much to the left and too short right now) using hints and whatnot - fine;
     if we *do* need to make the pipeline longer - change this to true and code the loader/converter...
   */
  override protected def isPipelineLong: Boolean = false

  override protected def imageLoaderFactory: ImageLoaderFactory = new AbstractImageLoaderFactory {
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

  override protected def imageConverter: ImageConverter = new AbstractImageConverter {
    override def getSourceFlavor: ImageFlavor = ImageFlavor.XML_DOM
    override def getTargetFlavor: ImageFlavor = ImageFlavor.GRAPHICS2D

    override def convert(src: Image, hints: java.util.Map[_, _]): Image = {
      val mathmlDocument: Document = src.asInstanceOf[ImageXMLDOM].getDocument
      val svgDocument: SVGDocument = typeset(mathmlDocument)
      val sizes: Sizes = Sizes(svgDocument)

      new ImageGraphics2D(src.getInfo, new Graphics2DImagePainter {
        override def getImageSize: Dimension = sizes.getDimension

        override def paint(graphics2d: Graphics2D, rectangle2d: Rectangle2D): Unit = {
          //val x: Float = 0
          //val y: Float = sizes.ascent
          val  hints: RenderingHints = graphics2d.getRenderingHints
          hints.add(new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON))
          hints.add(new RenderingHints(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE))
          hints.add(new RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY))
          graphics2d.setRenderingHints(hints)

          // see org.apache.fop.image.loader.batik.ImageConverterSVG2G2D for conversion code - but:
          // "Specialized renderers may want to provide specialized adapters to profit
          // from target-format features (for example with PDF or PS)."
        }
      })
    }
  }
}
