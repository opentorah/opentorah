package org.podval.docbook.gradle.mathjax

import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import org.apache.fop.fo.{ElementMapping, FONode}
import org.apache.fop.render.{Renderer, RendererContext, XMLHandler}
import org.apache.xmlgraphics.image.loader.{ImageContext, ImageInfo}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImagePreloader, ImageXMLDOM}
import org.apache.xmlgraphics.image.loader.spi.{ImageConverter, ImageLoaderFactory, ImagePreloader}
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.{DOMImplementation, Document}

//  Inspired by the JEuclid FOP plugin.
final class FopPlugin(mathJax: Typesetter) extends org.podval.docbook.gradle.fop.FopPlugin {

  def typeset(mathMLDocument: Document): SVGDocument = mathJax.typeset(mathMLDocument)

  override protected def elementMapping: ElementMapping = new ElementMapping {
    namespaceURI = MathML.Namespace.uri

    override def getDOMImplementation: DOMImplementation = ElementMapping.getDefaultDOMImplementation

    override protected def initialize(): Unit = {
      if (foObjs == null) {
        foObjs = new java.util.HashMap

        foObjs.put(MathML.math, new ElementMapping.Maker {
          override def make(parent: FONode): FONode = new MathML(parent, FopPlugin.this)
        })
        foObjs.put(ElementMapping.DEFAULT, new ElementMapping.Maker {
          override def make(parent: FONode): FONode = new MathML.Obj(parent)
        })
      }
    }
  }

  // Note: Not sure if I need this - it is probably used to read MathML from a file...
  // If I end up using it, make sure SVG rendering works properly...
  override protected def isHandlerNeeded: Boolean = false

  override protected def xmlHandler: XMLHandler = new XMLHandler() {
    override def getNamespace: String = MathML.Namespace.uri
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

      if (!MathML.Namespace.is(document)) null else  {
        val svgDocument: SVGDocument = typeset(document)
        val sizes: Sizes = Sizes(svgDocument)
        // convert sizes from exs to points:
        sizes.setViewPortSizes(svgDocument)

        val result: ImageInfo = new ImageInfo(uri, Svg.mimeType)
        result.setSize(sizes.getImageSize(context.getSourceResolution))

        // Stash the result to avoid typesetting again:
        result
          .getCustomObjects.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
          .put(ImageInfo.ORIGINAL_IMAGE, new ImageXMLDOM(result, svgDocument, Svg.Namespace.uri))

        result
      }
    }
  }

  // PreloaderMathML typesets MathML to SVG and hands it over to FOP for rendering, so we do not need the rest:
  override protected def isPipelineLong: Boolean = false
  override protected def imageLoaderFactory: ImageLoaderFactory = ???
  override protected def imageConverter: ImageConverter = ???
}
