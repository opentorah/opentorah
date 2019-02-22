package org.podval.docbook.gradle

import org.gradle.api.{DefaultTask, Project}
import org.gradle.api.file.CopySpec
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, TaskAction}
import java.io.File
import javax.xml.transform.stream.{StreamResult, StreamSource}
import org.apache.tools.ant.filters.ReplaceTokens
import org.xml.sax.InputSource

import scala.beans.BeanProperty
import scala.collection.JavaConverters._

class ProcessDocBookTask extends DefaultTask {

  private val layout: Layout = Layout.forProject(getProject)

  private val logger: Logger = new Logger.PluginLogger(getLogger)

  // To let projects that use the plugin to not make assumptions about directory names:
  @Internal def getOutputDirectory: File = layout.outputRoot

  // Register inputs
  val inputDirectories: Set[File] = Set(
    layout.inputDirectory,
    layout.cssDirectory,
    layout.fopConfigurationDirectory,
    layout.dataDirectory,
    layout.stylesheetDirectory
  ) ++ Set(
    layout.imagesDirectory,
  ).filter(_.exists)

  inputDirectories.foreach { directory: File =>
    logger.info(s"Registering input directory $directory")
    directory.mkdirs()
    getOutputs.dir(directory)
  }

  // Register outputs
  Set(
    layout.intermediateRoot,
    layout.outputRoot
  ).foreach { directory: File =>
    Util.deleteRecursively(directory)
    logger.info(s"Registering output directory $directory")
    getOutputs.dir(directory)
  }

  @Input @BeanProperty val document: Property[String] =
    getProject.getObjects.property(classOf[String])

  @BeanProperty val parameters: MapProperty[String, java.util.Map[String, String]] =
    getProject.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val outputFormats: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val isJEuclidEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @TaskAction
  def processDocBook(): Unit = {
    val documentName: String = Util.dropAllowedExtension(document.get, "xml")

    val processors: List[DocBook2] =
      Option(getProject.findProperty("docBook.outputFormats"))
        .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
        .getOrElse(outputFormats.get.asScala.toList)
        .map(forName)

    logger.info(s"Output formats: ${getNames(processors)}")

    val allParameters: Map[String, Map[String, String]] =
      parameters.get.asScala.toMap.mapValues(_.asScala.toMap)

    val unclaimedParameterSections: Set[String] = Util.unclaimedParameterSections(allParameters, processors.toSet)
    if (unclaimedParameterSections.nonEmpty)
      logger.info(s"Unclaimed parameter sections: ${unclaimedParameterSections.mkString(", ")}")

    // In processing instructions and CSS, substitute xslParameters also - because why not?
    val allSubstitutions: Map[String, String] =
      allParameters.values.toList.flatten.toMap ++ substitutions.get.asScala.toMap

    val resolver: Resolver = new Resolver(layout.catalogFile,  logger)

    processors.foreach { docBook2: DocBook2 => run(
      docBook2 = docBook2,
      documentName = documentName,
      substitutions = allSubstitutions,
      resolver = resolver
    )}
  }

  private def forName(name: String): DocBook2 = {
    val supported: List[DocBook2] = DocBook2.processors
    supported.find(processor => processor.name.equalsIgnoreCase(name)).getOrElse {
      throw new IllegalArgumentException(
        s"""Unsupported output format $name;
           |  supported formats are: ${getNames(supported)}""".stripMargin
      )
    }
  }

  private def getNames(processors: List[DocBook2]): String =
    "[" + processors.map(processor => "\"" + processor.name +"\"").mkString(", ") + "]"

  final def run(
    docBook2: DocBook2,
    documentName: String,
    substitutions: Map[String, String],
    resolver: Resolver
  ): Unit = {
    logger.lifecycle(s"DocBook: processing '$documentName' to ${docBook2.name}.")

    // Saxon output directory and file.
    val saxonOutputDirectory: File = layout.saxonOutputDirectory(docBook2)
    saxonOutputDirectory.mkdirs

    val saxonOutputFile: File = layout.saxonOutputFile(docBook2, documentName)
    val outputTarget = new StreamResult
    if (!docBook2.usesHtml) // skip outputTarget when chunking
      outputTarget.setSystemId(saxonOutputFile)

    // Saxon
    Saxon.run(
      inputSource = new InputSource(layout.inputFile(documentName).toURI.toASCIIString),
      stylesheetSource = new StreamSource(layout.stylesheetFile(layout.mainStylesheet(docBook2.stylesheetName))),
      outputTarget = outputTarget,
      resolver = resolver,
      processingInstructionsSubstitutions = substitutions,
      useXslt2 = docBook2.usesDocBookXslt2,
      logger = logger
    )

    val copyDestinationDirectory: File =
      Util.subdirectory(saxonOutputDirectory, docBook2.copyDestinationDirectoryName)

    // Images.
    logger.info(s"Copying images")
    getProject.copy((copySpec: CopySpec) => copySpec
      .into(copyDestinationDirectory)
      .from(layout.imagesDirectory.getParentFile)
      .include(layout.imagesDirectoryName + "/**")
    )

    // CSS.
    if (docBook2.usesCss) {
      logger.info(s"Copying CSS")
      getProject.copy((copySpec: CopySpec) => copySpec
        .into(copyDestinationDirectory)
        .from(layout.cssDirectory.getParentFile)
        .include(layout.cssDirectoryName + "/**")
        .filter(Map("tokens" -> substitutions.asJava).asJava, classOf[ReplaceTokens])
      )
    }

    // Post-processing.
    if (docBook2.usesIntermediate) {
      logger.info(s"Post-processing ${docBook2.name}")
      val outputDirectory: File = layout.outputDirectory(docBook2)
      outputDirectory.mkdirs

      docBook2.postProcess(
        layout = layout,
        isJEuclidEnabled = isJEuclidEnabled.get,
        inputDirectory = saxonOutputDirectory,
        inputFile = saxonOutputFile,
        outputFile = layout.outputFile(docBook2, documentName),
        logger = logger
      )
    }
  }
}
