package org.opentorah.fop

import java.io.{File, IOException, InputStream}
import java.net.URI
import org.apache.fop.apps.{FopConfParser, FopFactory, FopFactoryConfig}
import org.apache.fop.configuration.Configuration
import org.apache.xmlgraphics.image.loader.spi.ImageImplRegistry
import org.apache.xmlgraphics.image.loader.{ImageContext, ImageManager}
import org.xml.sax.SAXException

/* Create FopFactory with no plugin cross-contamination.
   FopFactory[Builder] methods that return FopFactory instances that use global ImageImplRegistry instance.
   That was probably done to optimize the discovery of the available services.
   To allow different FopFactory instances to use different plugins that register different image preloaders etc.,
   each factory has to get its own ImageImplRegistry instance.

   Since ImageImplRegistry is not settable on the ImageManager and
   Imagemanager is not settable on the FopFactory or FopFactoryConfig,
   I had to create a FopFactoryConfigProxy class (see below) that forwards all methods except getImageManager,
   where it supplies a fresh instance of ImageImplRegistry to each factory.
 */
object FopFactoryFactory {

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  // Uses configurationFile to determine baseUri
  def newFactory(configurationFile: File): FopFactory =
    newFactory(configurationFile, configurationFile)

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def newFactory(baseURI: URI, configurationStream: InputStream): FopFactory =
    newFactory(new FopConfParser(configurationStream, baseURI))

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def newFactory(configurationFile: File, inputFile: File): FopFactory =
    newFactory(new FopConfParser(configurationFile, inputFile.getParentFile.toURI))

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def newFactory(configurationParser: FopConfParser): FopFactory =
    newFactory(configurationParser.getFopFactoryBuilder.buildConfig)

  def newFactory(configuration: FopFactoryConfig): FopFactory =
    FopFactory.newInstance(new FopFactoryConfigProxy(configuration))


  class FopFactoryConfigProxy(delegate: FopFactoryConfig) extends FopFactoryConfig {
    private val imageManager: ImageManager = new ImageManager(
      new ImageImplRegistry,
      new ImageContext() {
        def getSourceResolution: Float = delegate.getSourceResolution
      }
    )

    override def getImageManager: ImageManager = imageManager

    override def isAccessibilityEnabled: Boolean = delegate.isAccessibilityEnabled
    override def isKeepEmptyTags: Boolean = delegate.isKeepEmptyTags
    override def getLayoutManagerMakerOverride: org.apache.fop.layoutmgr.LayoutManagerMaker = delegate.getLayoutManagerMakerOverride
    override def getResourceResolver: org.apache.xmlgraphics.io.ResourceResolver = delegate.getResourceResolver
    override def getBaseURI: URI = delegate.getBaseURI
    override def getHyphenationResourceResolver: org.apache.fop.apps.io.InternalResourceResolver = delegate.getHyphenationResourceResolver
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
    override def getFontManager: org.apache.fop.fonts.FontManager = delegate.getFontManager
    override def isComplexScriptFeaturesEnabled: Boolean = delegate.isComplexScriptFeaturesEnabled
    override def getHyphenationPatternNames: java.util.Map[String, String] = delegate.getHyphenationPatternNames
    override def getFallbackResolver: org.apache.xmlgraphics.image.loader.impl.AbstractImageSessionContext.FallbackResolver = delegate.getFallbackResolver
  }
}
