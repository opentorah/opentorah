package org.podval.docbook.gradle

import org.gradle.api.{Action, Project}
import org.gradle.api.file.CopySpec
import java.io.File
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.apache.tools.ant.filters.ReplaceTokens
import org.gradle.api.artifacts.Configuration
import org.xml.sax.InputSource
import scala.collection.JavaConverters._

abstract class DocBook2 {

  final def run(
    layout: Layout,
    inputFileName: String,
    saxon: Saxon,
    xslParameters: Map[String, String],
    substitutions: Map[String, String],
    project: Project,
    logger: Logger
  ): Unit = {
    logger.info(s"\nProcessing DocBook to $finalOutputFormat")
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

    logger.info(s"Copying images")
    project.copy(new Action[CopySpec] {
      override def execute(copySpec: CopySpec): Unit = {
        copySpec
          .into(saxonOutputDirectory)
          .from(layout.imagesDirectory.getParentFile)
          .include(layout.imagesDirectoryName + "/**")
      }
    })

    val outputDirectory: File = finalOutputDirectory(layout)
    outputDirectory.mkdirs

    postProcess(
      layout = layout,
      inputFileName = inputFileName,
      substitutions = substitutions ++ xslParameters,
      project = project,
      logger = logger
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
    "html.stylesheet" -> (layout.cssDirectoryName + "/" + layout.cssFileName)
  )

  protected def postProcess(
    layout: Layout,
    inputFileName: String,
    substitutions: Map[String, String],
    project: Project,
    logger: Logger
  ): Unit = {
  }

  protected final def copyCss(
    layout: Layout,
    directory: File,
    substitutions: Map[String, String],
    project: Project,
    logger: Logger
  ): Unit = {
    logger.info(s"Copying CSS")
    project.copy(new Action[CopySpec] {
      override def execute(copySpec: CopySpec): Unit = {
        copySpec
          .into(directory)
          .from(layout.cssDirectory.getParentFile)
          .include(layout.cssDirectoryName + "/**")
          .filter(Map("tokens" -> substitutions.asJava).asJava, classOf[ReplaceTokens])
      }
    })
  }
}

object DocBook2 {

  val processors: List[DocBook2] = List(DocBook2Html, DocBook2Epub, DocBook2Pdf)

  def process(
    outputFormats: List[String],
    layout: Layout,
    inputFileName: String,
    xslParameters: Map[String, String],
    substitutions: Map[String, String],
    project: Project,
    logger: Logger
  ): Unit = {
    val processorsToRun: List[DocBook2] = outputFormats.map { outputFormat =>
      val processor: Option[DocBook2] = processors.find(_.finalOutputFormat == outputFormat)
      if (processor.isEmpty) {
        val message: String =
          s"""Unsupported output format $outputFormat;
             |  supported formats are: ${processors.map(_.finalOutputFormat)}""".stripMargin
        logger.error(message)
        throw new IllegalArgumentException(message)
      } else {
        processor.get
      }
    }

    logger.info(s"Output formats selected: ${processorsToRun.map(_.finalOutputFormat)}")

    val docBookXslConfiguration: Configuration = project.getConfigurations.findByName("docBookXsl")

    // Unpack DocBook XSLT stylesheets
    if (!layout.docBookXslDirectory.exists) {
      logger.info(s"Preparing DocBook XSLT stylesheets")
      project.copy(new Action[CopySpec] {
        override def execute(copySpec: CopySpec): Unit = {
          copySpec
            .into(layout.explodeDocBookXslInto)
            .from(project.zipTree(docBookXslConfiguration.getSingleFile))
        }
      })
    }

    val saxon: Saxon = new Saxon(
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
      substitutions = substitutions,
      project = project,
      logger = logger
    ))
  }
}
