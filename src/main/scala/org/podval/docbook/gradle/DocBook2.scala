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
    xslParameters: Map[String, String],
    substitutions: Map[String, String],
    epubEmbeddedFonts: List[String],
    project: Project,
    logger: Logger
  ): Unit = {
    logger.info(s"\nProcessing DocBook to $name")

    // Saxon output directory and file.
    val saxonOutputDirectory: File = new File(
      if (usesIntermediate) layout.intermediateOutputDirectoryRoot else layout.finalOutputDirectoryRoot,
      if (usesIntermediate) intermediateDirectoryName else outputDirectoryName
    )

    val outputFileName: String = outputFileNameOverride.getOrElse(inputFileName)
    def outputFile(directory: File, extension: String): File = new File(directory, outputFileName + "." + extension)

    val saxonOutputFile: File = outputFile(saxonOutputDirectory,
      if (usesIntermediate) intermediateFileExtension else outputFileExtension
    )

    saxonOutputDirectory.mkdirs

    // Image and HTML-related XSL parameters.
    val additionalParameters: Map[String, String] =
      Map(
        "img.src.path" -> (layout.imagesDirectoryName + "/"),
        "epub.embedded.fonts" -> Fop.getFontFiles(layout.fopConfigurationFile, epubEmbeddedFonts, logger)
      ) ++
        (if (!usesHtml) Map.empty else Map(
          "base.dir" -> saxonOutputDirectory.getAbsolutePath,
          "root.filename" -> Util.fileNameWithoutExtension(saxonOutputFile),
          "html.stylesheet" -> (layout.cssDirectoryName + "/" + layout.cssFileName)
        ))

    val xslParametersEffective: Map[String, String] =
      xslParameters ++ (additionalParameters -- xslParameters.keySet)

    // In processing instructions and CSS, substitute xslParameters also - because why not?
    val allSubstitutions: Map[String, String] = substitutions ++ xslParametersEffective

    // Saxon
    Saxon.run(
      inputSource = new InputSource(layout.inputFile(inputFileName).toURI.toASCIIString),
      stylesheetSource = new StreamSource(layout.stylesheetFile(stylesheetName)),
      outputTarget = new StreamResult(saxonOutputFile),
      xslParameters = xslParametersEffective,
      entitySubstitutions = substitutions,
      processingInstructionsSubstitutions = allSubstitutions,
      xslDirectory = layout.docBookXslDirectory,
      dataDirectory = layout.dataDirectory,
      logger = logger
    )

    val copyDestinationDirectory: File =
      copyDestinationDirectoryName.fold(saxonOutputDirectory)(new File(saxonOutputDirectory, _))

    // Images.
    logger.info(s"Copying images")
    project.copy(new Action[CopySpec] {
      override def execute(copySpec: CopySpec): Unit = {
        copySpec
          .into(copyDestinationDirectory)
          .from(layout.imagesDirectory.getParentFile)
          .include(layout.imagesDirectoryName + "/**")
      }
    })

    // CSS.
    if (usesCss) {
      logger.info(s"Copying CSS")
      project.copy(new Action[CopySpec] {
        override def execute(copySpec: CopySpec): Unit = {
          copySpec
            .into(copyDestinationDirectory)
            .from(layout.cssDirectory.getParentFile)
            .include(layout.cssDirectoryName + "/**")
            .filter(Map("tokens" -> allSubstitutions.asJava).asJava, classOf[ReplaceTokens])
        }
      })
    }

    // Post-processing.
    if (usesIntermediate) {
      logger.info(s"Post-processing $name")
      val outputDirectory: File = new File(layout.finalOutputDirectoryRoot, outputDirectoryName)
      outputDirectory.mkdirs

      postProcess(
        layout = layout,
        inputDirectory = saxonOutputDirectory,
        inputFile = saxonOutputFile,
        outputFile = outputFile(outputDirectory, outputFileExtension),
        logger = logger
      )
    }
  }

  def usesHtml: Boolean

  def usesCss: Boolean

  def usesIntermediate: Boolean

  def stylesheetName: String

  def outputDirectoryName: String
  def outputFileExtension: String

  def intermediateDirectoryName: String = outputDirectoryName
  def intermediateFileExtension: String = outputFileExtension

  def outputFileNameOverride: Option[String] = None

  def copyDestinationDirectoryName: Option[String] = None

  protected def postProcess(
    layout: Layout,
    inputDirectory: File,
    inputFile: File,
    outputFile: File,
    logger: Logger
  ): Unit = {
  }
}

object DocBook2 {

  object Html extends DocBook2 {
    override def usesHtml: Boolean = true
    override def usesCss: Boolean = true
    override def usesIntermediate: Boolean = false
    override def stylesheetName: String = "html"
    override def outputDirectoryName: String = "html"
    override def outputFileExtension: String = "html"
    override def outputFileNameOverride: Option[String] = Some("index")
  }

  object Pdf extends DocBook2 {
    override def usesHtml: Boolean = false
    override def usesCss: Boolean = false
    override def usesIntermediate: Boolean = true

    override def stylesheetName: String = "fo"
    override def intermediateDirectoryName: String = "fo"
    override def intermediateFileExtension: String = "fo"
    override def outputDirectoryName: String = "pdf"
    override def outputFileExtension: String = "pdf"

    override protected def postProcess(
      layout: Layout,
      inputDirectory: File,
      inputFile: File,
      outputFile: File,
      logger: Logger
    ): Unit = Fop.run(
      configurationFile = layout.fopConfigurationFile,
      inputFile = inputFile,
      baseDirectory = inputDirectory,
      outputFile = outputFile,
      logger = logger
    )
  }

  trait Epub extends DocBook2 {
    final override def usesHtml: Boolean = true
    final override def usesCss: Boolean = false
    final override def usesIntermediate: Boolean = true
    final override def outputFileExtension: String = "epub"
    final override def copyDestinationDirectoryName: Option[String] = Some("OEBPS")

    final override protected def postProcess(
      layout: Layout,
      inputDirectory: File,
      inputFile: File,
      outputFile: File,
      logger: Logger
    ): Unit = {
      val zip = new org.apache.tools.ant.taskdefs.Zip
      zip.setProject(new org.apache.tools.ant.Project)
      zip.setPreserve0Permissions(true)
      zip.setCompress(false)
      zip.setDestFile(outputFile)
      val fileSet = new org.apache.tools.ant.types.FileSet()
      fileSet.setDir(inputDirectory)
      fileSet.appendIncludes(Array("mimetype", "META-INF/**", "OEBPS/**"))
      zip.addFileset(fileSet)
      zip.execute()
    }
  }

  object Epub2 extends Epub {
    override def outputDirectoryName: String = "epub"
    override def stylesheetName: String = "epub"
  }

  object Epub3 extends Epub {
    override def outputDirectoryName: String = "epub3"
    override def stylesheetName: String = "epub3"
  }

  private val all: List[DocBook2] = List(Html, Epub2, Epub3, Pdf)

  def forName(name: String): DocBook2 = all.find(_.name.toUpperCase == name.toUpperCase).getOrElse {
    throw new IllegalArgumentException(
      s"""Unsupported output format $name;
         |  supported formats are: $availableFormatNames""".stripMargin
    )
  }

  val availableFormats: Seq[String] = all.map(_.name)

  val availableFormatNames: String = availableFormats.mkString(", ")
}
