package org.podval.docbook.gradle.fop

import net.sourceforge.jeuclid.fop.{JEuclidElementMapping, JEuclidXMLHandler}
import net.sourceforge.jeuclid.xmlgraphics.{ImageConverterMathML2G2D, ImageLoaderFactoryMathML, PreloaderMathML}
import org.apache.fop.fo.ElementMapping
import org.apache.fop.render.XMLHandler
import org.apache.xmlgraphics.image.loader.spi.{ImageConverter, ImageLoaderFactory, ImagePreloader}

final class JEuclidFopPlugin extends FopPlugin {
  override protected def elementMapping: ElementMapping = new JEuclidElementMapping
  override protected def isHandlerNeeded: Boolean = true
  override protected def xmlHandler: XMLHandler = new JEuclidXMLHandler
  override protected def imagePreloader: ImagePreloader = new PreloaderMathML
  override protected def isPipelineLong: Boolean = true
  override protected def imageLoaderFactory: ImageLoaderFactory = new ImageLoaderFactoryMathML
  override protected def imageConverter: ImageConverter = new ImageConverterMathML2G2D
}
