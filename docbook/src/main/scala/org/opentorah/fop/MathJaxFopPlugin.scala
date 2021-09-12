package org.opentorah.fop

import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import org.apache.fop.fo.{ElementMapping, FONode}
import org.apache.fop.render.{Renderer, RendererContext, XMLHandler}
import org.apache.xmlgraphics.image.loader.impl.{AbstractImagePreloader, ImageXMLDOM}
import org.apache.xmlgraphics.image.loader.spi.{ImageConverter, ImageLoaderFactory, ImagePreloader}
import org.apache.xmlgraphics.image.loader.{ImageContext, ImageInfo}
import org.opentorah.mathjax.MathML
import org.opentorah.xml.Dom
import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.{DOMImplementation, Document}

//  Inspired by the JEuclid FOP plugin.
final class MathJaxFopPlugin(mathJax: MathJaxRunner) extends FopPlugin:

  override protected def elementMapping: ElementMapping = new ElementMapping:
    namespaceURI = MathML.namespace.uri

    override def getDOMImplementation: DOMImplementation = ElementMapping.getDefaultDOMImplementation

    override protected def initialize(): Unit =
      if foObjs == null then
        foObjs = new java.util.HashMap

        foObjs.put(MathML.math, new ElementMapping.Maker:
          override def make(parent: FONode): FONode = MathMLObj(parent, mathJax)
        )
        foObjs.put(ElementMapping.DEFAULT, new ElementMapping.Maker:
          override def make(parent: FONode): FONode = MathMLObj.Obj(parent)
        )

  // Note: Not sure what is this for - it is not even needed to read MathML from a file...
  // If I end up using it, make sure SVG rendering works properly...
  override protected def isHandlerNeeded: Boolean = false

  override protected def xmlHandler: XMLHandler = new XMLHandler():
    override def getNamespace: String = MathML.namespace.uri
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
      mathJax.typeset(document),
      ns
    )

  override protected def imagePreloader: ImagePreloader = new AbstractImagePreloader:
    override def preloadImage(uri: String, src: Source, context: ImageContext): ImageInfo =
      if !src.isInstanceOf[DOMSource] then null else
        val document: Document = src.asInstanceOf[DOMSource].getNode.asInstanceOf[Document]

        if MathML.namespace.getUri != Dom.getNamespace(document.getDocumentElement).getUri then null else
          val svgDocument: SVGDocument = mathJax.typeset(document)
          val sizes: Sizes = Sizes(svgDocument)
          // convert sizes from exs to points:
          sizes.setViewPortSizes(svgDocument)

          val result: ImageInfo = ImageInfo(uri, Svg.mimeType)
          result.setSize(sizes.getImageSize(context.getSourceResolution))

          // Stash the result to avoid typesetting again:
          result
            .getCustomObjects.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
            .put(ImageInfo.ORIGINAL_IMAGE, ImageXMLDOM(result, svgDocument, Svg.namespace.uri))

          result

  // PreloaderMathML typesets MathML to SVG and hands it over to FOP for rendering, so we do not need the rest:
  override protected def isPipelineLong: Boolean = false
  override protected def imageLoaderFactory: ImageLoaderFactory = ???
  override protected def imageConverter: ImageConverter = ???
