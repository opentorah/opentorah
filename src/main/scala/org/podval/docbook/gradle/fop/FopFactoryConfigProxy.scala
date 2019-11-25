package org.podval.docbook.gradle.fop

import java.net.URI

import org.apache.fop.apps.FopFactoryConfig
import org.apache.fop.apps.io.InternalResourceResolver
import org.apache.fop.configuration.Configuration
import org.apache.fop.fonts.FontManager
import org.apache.fop.layoutmgr.LayoutManagerMaker
import org.apache.xmlgraphics.image.loader.impl.AbstractImageSessionContext.FallbackResolver
import org.apache.xmlgraphics.image.loader.spi.ImageImplRegistry
import org.apache.xmlgraphics.image.loader.{ImageContext, ImageManager}
import org.apache.xmlgraphics.io.ResourceResolver

class FopFactoryConfigProxy(delegate: FopFactoryConfig) extends FopFactoryConfig {
  override def isAccessibilityEnabled: Boolean = delegate.isAccessibilityEnabled
  override def getLayoutManagerMakerOverride: LayoutManagerMaker = delegate.getLayoutManagerMakerOverride
  override def getResourceResolver: ResourceResolver = delegate.getResourceResolver
  override def getBaseURI: URI = delegate.getBaseURI
  override def getHyphenationResourceResolver: InternalResourceResolver = delegate.getHyphenationResourceResolver
  override def validateStrictly: Boolean = delegate.validateStrictly
  override def validateUserConfigStrictly: Boolean = delegate.validateUserConfigStrictly
  override def isBreakIndentInheritanceOnReferenceAreaBoundary: Boolean = delegate.isBreakIndentInheritanceOnReferenceAreaBoundary
  override def getSourceResolution: Float = delegate.getSourceResolution
  override def getTargetResolution: Float = delegate.getTargetResolution
  override def getPageHeight: String = delegate.getPageHeight
  override def getPageWidth: String = delegate.getPageWidth
  override def getIgnoredNamespaces: java.util.Set[String] = delegate.getIgnoredNamespaces
  override def isNamespaceIgnored(namespace: String): Boolean = delegate.isNamespaceIgnored(namespace)
  override def getUserConfig: Configuration = delegate.getUserConfig
  override def preferRenderer: Boolean = delegate.preferRenderer
  override def getFontManager: FontManager = delegate.getFontManager
  override def isComplexScriptFeaturesEnabled: Boolean = delegate.isComplexScriptFeaturesEnabled
  override def getHyphenationPatternNames: java.util.Map[String, String] = delegate.getHyphenationPatternNames
  override def getFallbackResolver: FallbackResolver = delegate.getFallbackResolver

  /* Note: Override imageManager with one where ImageImplRegistry is *not* the (global!) defaultInstance,
     but a fresh one (scoped by this object), so that preloaders etc. registered on one FopFactory
     do not interfere with another.
     I had to tweak the process at this point since ImageManager is not settable on the FopFactory or FopFactoryConfig
     and ImageImplRegistry is not settable on the ImageManager.
     Fop people use a global (likely to optimize the discovery of the available services) and didn't make it easy to
     counteract its effects...
   */
  private val imageManager: ImageManager = new ImageManager(
    new ImageImplRegistry,
    new ImageContext() {
      def getSourceResolution: Float = delegate.getSourceResolution
    }
  )

  override def getImageManager: ImageManager = imageManager
}
