package org.podval.docbook.gradle

import org.gradle.api.{Action, Project}
import org.gradle.api.file.CopySpec
import java.io.File
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.apache.tools.ant.filters.ReplaceTokens
import org.xml.sax.InputSource
import scala.collection.JavaConverters._

abstract class DocBook2 {

  final def name: String = Util.className(this).toUpperCase

  final def run(
    layout: Layout,
    inputFileName: String,
    saxon: Saxon,
    xslParameters: Map[String, String],
    substitutions: Map[String, String],
    project: Project,
    logger: Logger
  ): Unit = {
    logger.info(s"\nProcessing DocBook to $name")

    val saxonOutputDirectory: File = new File(
      if (saxon2intermediate) layout.intermediateOutputDirectoryRoot else layout.finalOutputDirectoryRoot,
      saxonOutputFormat
    )
    saxonOutputDirectory.mkdirs

    val saxonOutputFile: File = outputFileFor(saxonOutputDirectory, saxonOutputFormat, inputFileName)

    val additionalParameters: Map[String, String] =
      Map("img.src.path" -> (layout.imagesDirectoryName + "/")) ++
        (if (!usesHtml) Map.empty else Map(
          "base.dir" -> saxonOutputDirectory.getAbsolutePath,
          "root.filename" -> Util.fileNameWithoutExtension(saxonOutputFile),
          "html.stylesheet" -> (layout.cssDirectoryName + "/" + layout.cssFileName)
        ))

    val xslParametersEffective: Map[String, String] =
      xslParameters ++ (additionalParameters -- xslParameters.keySet)

    saxon.run(
      inputSource = new InputSource(layout.inputFile(inputFileName).toURI.toASCIIString),
      stylesheetSource = new StreamSource(layout.stylesheetFile(saxonOutputFormat)),
      outputTarget = new StreamResult(saxonOutputFile),
      xslParameters = xslParametersEffective
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

    if (usesHtml) {
      val substitutionsEffective: Map[String, String] = substitutions ++ xslParametersEffective
      logger.info(s"Copying CSS")
      project.copy(new Action[CopySpec] {
        override def execute(copySpec: CopySpec): Unit = {
          copySpec
            .into(saxonOutputDirectory)
            .from(layout.cssDirectory.getParentFile)
            .include(layout.cssDirectoryName + "/**")
            .filter(Map("tokens" -> substitutionsEffective.asJava).asJava, classOf[ReplaceTokens])
        }
      })
    }

    if (saxon2intermediate) {
      logger.info(s"Post-processing $name")
      val outputDirectory: File = new File(layout.finalOutputDirectoryRoot, finalOutputFormat)
      outputDirectory.mkdirs

      postProcess(
        layout = layout,
        saxonOutputDirectory = saxonOutputDirectory,
        saxonOutputFile = saxonOutputFile,
        outputFile = outputFileFor(outputDirectory, finalOutputFormat, inputFileName),
        logger = logger
      )
    }
  }

  private def outputFileFor(outputDirectory: File, outputFormat: String, inputFileName: String): File = {
    val outputFileName: String = outputFileNameOverride.getOrElse(inputFileName)
    new File(outputDirectory, outputFileName + "." + outputFormat)
  }

  def usesHtml: Boolean

  def saxon2intermediate: Boolean

  def finalOutputFormat: String

  def saxonOutputFormat: String = finalOutputFormat

  protected def outputFileNameOverride: Option[String] = None

  protected def postProcess(
    layout: Layout,
    saxonOutputDirectory: File,
    saxonOutputFile: File,
    outputFile: File,
    logger: Logger
  ): Unit = {
  }
}

object DocBook2 {

  object Html extends DocBook2 {
    override def usesHtml: Boolean = true
    override def saxon2intermediate: Boolean = false
    override def finalOutputFormat: String = "html"
    override protected def outputFileNameOverride: Option[String] = Some("index")
  }

  object Pdf extends DocBook2 {
    override def usesHtml: Boolean = false
    override def saxon2intermediate: Boolean = true
    override def saxonOutputFormat: String = "fo"
    override def finalOutputFormat: String = "pdf"

    override protected def postProcess(
      layout: Layout,
      saxonOutputDirectory: File,
      saxonOutputFile: File,
      outputFile: File,
      logger: Logger
    ): Unit = Fop.run(
      configurationFile = layout.fopConfigurationFile,
      inputFile = saxonOutputFile,
      baseDirectory = saxonOutputDirectory,
      outputFile = outputFile,
      logger = logger
    )
  }

  object Epub2 extends DocBook2 {
    override def usesHtml: Boolean = true
    override def saxon2intermediate: Boolean = true
    override def finalOutputFormat: String = "epub"

    override protected def postProcess(
      layout: Layout,
      saxonOutputDirectory: File,
      saxonOutputFile: File,
      outputFile: File,
      logger: Logger
    ): Unit = Epub.pack(
      inputDirectory = saxonOutputDirectory,
      outputFile = outputFile
    )
  }

  private val all: List[DocBook2] = List(Html, Epub2, Pdf)

  def forName(name: String): DocBook2 = all.find(_.name.toUpperCase == name.toUpperCase).getOrElse {
    throw new IllegalArgumentException(
      s"""Unsupported output format $name;
         |  supported formats are: $availableFormatNames""".stripMargin
    )
  }

  val availableFormats: Seq[String] = all.map(_.name)

  val availableFormatNames: String = availableFormats.mkString(", ")
}
