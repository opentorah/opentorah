package org.opentorah.fop

import org.apache.fop.apps.FopFactory
import org.apache.fop.fo.ElementMapping
import org.apache.fop.render.XMLHandler
import org.apache.xmlgraphics.image.loader.spi.{ImageConverter, ImageImplRegistry, ImageLoaderFactory, ImagePreloader}

trait FopPlugin:

  final def configure(fopFactory: FopFactory): Unit =
    fopFactory.getElementMappingRegistry.addElementMapping(elementMapping)

    if isHandlerNeeded then
      fopFactory.getXMLHandlerRegistry.addXMLHandler(xmlHandler)

    val images: ImageImplRegistry = fopFactory.getImageManager.getRegistry

    images.registerPreloader(imagePreloader)

    if isPipelineLong then
      images.registerLoaderFactory(imageLoaderFactory)
      images.registerConverter(imageConverter)

  protected def elementMapping: ElementMapping

  protected def isHandlerNeeded: Boolean

  protected def xmlHandler: XMLHandler

  protected def imagePreloader: ImagePreloader

  protected def isPipelineLong: Boolean

  protected def imageLoaderFactory: ImageLoaderFactory

  protected def imageConverter: ImageConverter
