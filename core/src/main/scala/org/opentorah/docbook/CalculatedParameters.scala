package org.opentorah.docbook

import org.opentorah.math.MathConfiguration
import org.opentorah.xml.ScalaXml
import org.slf4j.Logger
import java.io.File

final class CalculatedParameters(
  layout: Layout,
  documentName: String,
  format: XsltFormat,
  saxonOutput: File,
  math: MathConfiguration,
  epubEmbeddedFontsString: Option[String],
  logger: Logger
):
  def epubEmbeddedFonts: Map[String, String] =
    epubEmbeddedFontsString.fold(Map.empty)(epubEmbeddedFontsString => Map("epub.embedded.fonts" -> epubEmbeddedFontsString))

  def rootFilename: (String, String) = "root.filename" -> format.rootFilename(documentName)
  def htmlStylesheet: (String, String) = "html.stylesheet" -> (Layout.cssDirectory + "/" + Layout.cssFile)
  def imgSrcPath: (String, String) = "img.src.path" -> (Layout.imagesDirectory + "/")
  def mathJaxConfigurationParameter: Map[String, String] = if !math.enableMathJax then Map.empty else Map(
    CalculatedParameters.mathConfigurationParameterName -> math.mathJax.htmlConfigurationString(math)
  )
  // TODO
  // - do it directly, without additional parameter;
  // - integrate withe the rest of the Site hed contributions;
  // - do the same for header and footer.
  def mathJaxUserHeadContent: ScalaXml.Nodes = if !math.enableMathJax then Seq.empty else Seq(
      <!-- Add MathJax support -->,
      <xsl:template name="user.head.content">
        {math.mathJax.body(<xsl:value-of select={s"$$${CalculatedParameters.mathConfigurationParameterName}"}/>)}
      </xsl:template>
    )
  def baseDir: (String, String) = "base.dir" -> (saxonOutput.getAbsolutePath + "/")
  def chunkQuietly: (String, String) = "chunk.quietly" -> (if logger.isInfoEnabled then "0" else "1")


object CalculatedParameters:
  private val mathConfigurationParameterName: String = "mathJax.configuration"
