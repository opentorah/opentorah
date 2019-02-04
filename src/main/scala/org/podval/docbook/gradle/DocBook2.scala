package org.podval.docbook.gradle

import org.gradle.api.{Action, Project}
import org.gradle.api.file.CopySpec
import java.io.File
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.apache.tools.ant.filters.ReplaceTokens
import org.xml.sax.InputSource
import scala.collection.JavaConverters._

abstract class DocBook2 {

  final def run(
    layout: Layout,
    inputFileName: String,
    saxon: Saxon,
    xslParameters: Map[String, String],
    substitutions: Map[String, String],
    project: Project
  ): Unit = {
    val saxonOutputDirectory: File = intermediateOutputDirectory(layout)
    saxonOutputDirectory.mkdirs

    val saxonOutputFile: File = intermediateOutputFile(layout, inputFileName)

    val allAdditionalParameters: Map[String, String] =
      Map("img.src.path" -> (layout.imagesDirectoryName + "/")) ++
        additionalParameters(layout, inputFileName)

    saxon.run(
      inputSource = new InputSource(layout.inputFile(inputFileName).toURI.toASCIIString),
      stylesheetSource = new StreamSource(layout.stylesheetFile(saxonOutputFormat)),
      outputTarget = new StreamResult(saxonOutputFile),
      xslParameters = xslParameters ++ (allAdditionalParameters -- xslParameters.keySet)
    )

    val outputDirectory: File = finalOutputDirectory(layout)
    outputDirectory.mkdirs

    postProcess(
      layout,
      inputFileName,
      substitutions,
      project
    )
  }

  def saxon2intermediate: Boolean

  def finalOutputFormat: String

  def saxonOutputFormat: String = finalOutputFormat

  protected final def intermediateOutputDirectory(layout: Layout): File = new File(
    if (saxon2intermediate) layout.intermediateOutputDirectoryRoot else layout.finalOutputDirectoryRoot,
    saxonOutputFormat
  )

  protected final def finalOutputDirectory(layout: Layout): File =
    new File(layout.finalOutputDirectoryRoot, finalOutputFormat)

  protected final def intermediateOutputFile(layout: Layout, inputFileName: String): File =
    outputFile(intermediateOutputDirectory(layout), saxonOutputFormat, inputFileName)

  protected final def finalOutputFile(layout: Layout, inputFileName: String): File =
    outputFile(finalOutputDirectory(layout), finalOutputFormat, inputFileName)

  private def outputFile(outputDirectory: File, outputFormat: String, inputFileName: String): File =
    new File(outputDirectory, outputFileName(inputFileName) + "." + outputFormat)

  protected final def outputFileName(inputFileName: String): String = outputFileNameOverride.getOrElse(inputFileName)

  protected def outputFileNameOverride: Option[String] = None

  protected def additionalParameters(layout: Layout, inputFileName: String): Map[String, String] = Map.empty

  protected final def additionalParametersHtml(layout: Layout, inputFileName: String): Map[String, String] = Map(
    "base.dir" -> intermediateOutputDirectory(layout).getAbsolutePath,
    "root.filename" -> Util.fileNameWithoutExtension(intermediateOutputFile(layout, inputFileName)),
    "html.stylesheet" -> (layout.cssDirectoryName + "/docBook.css")
  )

  protected def postProcess(
    layout: Layout,
    inputFileName: String,
    substitutions: Map[String, String],
    project: Project
  ): Unit = {
  }

  protected final def copyImagesAndCss(
    layout: Layout,
    directory: File,
    substitutions: Map[String, String],
    project: Project
  ): Unit = {
    project.copy(new Action[CopySpec] {
      override def execute(copySpec: CopySpec): Unit = {
        copySpec
          .into(directory)
          .`with`(project.copySpec
            .from(layout.sourceRootDirectory)
            .include(layout.imagesDirectoryName + "/**"))
          .`with`(project.copySpec
            .from(layout.sourceRootDirectory)
            .include(layout.cssDirectoryName + "/**")
            .filter(Map("tokens" -> substitutions.asJava).asJava, classOf[ReplaceTokens]))
      }
    })
  }
}

object DocBook2 {

  val processors: List[DocBook2] = List(DocBook2Html, DocBook2Epub, DocBook2Pdf)

  val outputFormats: List[String] = processors.map(_.finalOutputFormat)

  def process(
    outputFormats: List[String],
    layout: Layout,
    inputFileName: String,
    xslParameters: Map[String, String],
    entities: Map[String, String],
    substitutions: Map[String, String],
    project: Project,
    logger: Logger
  ): Unit = {
    val processorsToRun: List[DocBook2] = outputFormats.map { outputFormat =>
      val processor: Option[DocBook2] = processors.find(_.finalOutputFormat == outputFormat)
      if (processor.isEmpty) {
        val message: String = s"Unsupported oitput format $outputFormat; supported formats are: ${processors.map(_.finalOutputFormat)}"
        logger.error(message)
        throw new IllegalArgumentException(message)
      } else {
        processor.get
      }
    }

    val saxon: Saxon = new Saxon(
      entities = entities,
      substitutions = substitutions,
      xslDirectory = layout.docBookXslDirectory,
      dataDirectory = layout.dataDirectory,
      logger: Logger
    )

    processorsToRun.foreach(_.run(
      layout = layout,
      inputFileName = inputFileName,
      saxon = saxon,
      xslParameters = xslParameters,
      substitutions = entities ++ substitutions,
      project = project
    ))
  }
}
